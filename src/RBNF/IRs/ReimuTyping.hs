-- | Type inference for Reimu IR
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-06
-- License: BSD-3-clause
module RBNF.IRs.ReimuTyping where

import RBNF.IRs.Reimu
import RBNF.IRs.Marisa (Marisa(..))
import RBNF.IRs.MarisaLibrary
import RBNF.HMTypeInfer

import RSolve.MapLike
import RSolve.PropLogic
import RSolve.MultiState
import RBNF.Utils
import RBNF.Name
import RBNF.TypeSystem

import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Applicative ((<|>))

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Data.Maybe (fromJust)

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L

-- import Debug.Trace

type T = HMT RT
type TCEnv = HMTCEnv RT
type Unif = HMUnif RT


-- | Each module will hold a 'TInfo'
data TInfo
    = TInfo {
          _prims      :: [(RT, Int)]
          -- | a map that maps a field name to
          -- candicate lists of (structTypec, fieldType)
        , _fields     :: Map String [(S.Set String, T, T)]
        , _constr     :: [WFF Unif] -- _constraints
        , maxTupleDim :: Int
        -- context sensitive
        , _types  :: Map MName T
        , _kinds  :: Map MName T
       }

emptyTInfo = TInfo [] M.empty [] 6 M.empty M.empty

makeLenses ''TInfo


toRT :: T -> MS (TCEnv TInfo) RT
toRT = \case
    a :# b   -> RTApp <$> toRT a <*> toRT b
    a :-> b -> RTFunc <$> toRT a <*> toRT b
    TForall xs t -> RTGeneric (S.toList xs) <$> toRT t
    TFresh a -> return $ RTVar a
    TVar i   -> return $ RTVar ("@" ++ show i)
    TNom i   -> return i
    a :* b   -> do
        hd <- toRT a
        let packTl = \case
                b1 :* b2 -> do
                    b1 <- toRT b1
                    (b1:) <$> packTl b2
                a -> do
                    a <- toRT a
                    case a of
                        RTPrim RTUnit -> return []
                        a -> return [a]
        tl <-  packTl b
        return $ RTTuple $ hd:tl

addPrim :: RTPrim -> Int -> TInfo -> TInfo
addPrim t i = over prims (insert (RTPrim t) i)

addField :: String -> S.Set String -> T -> T -> TInfo -> TInfo
addField fieldName freshvars tapp fieldType =
        over fields (M.insertWith (++) fieldName [(freshvars, tapp, fieldType)])

addType :: MName -> T -> TInfo -> TInfo
addType tname t = over types (M.insert tname t)

addTypeApp :: MName -> T -> TInfo -> TInfo
addTypeApp tname t = over kinds (M.insert tname t)

nTupleST :: [T] -> T
nTupleST [] = getPrim RTUnit
nTupleST (x:xs) =  foldl (:*) x xs

declareStruct :: MName -> [String] -> [(String, T)] ->  MS (TCEnv TInfo) T
declareStruct structName typeParams fields = do
    let freshvars = S.fromList typeParams
        struct  = TNom $ TSig structName
        tapp = struct :# nTupleST [TFresh p | p <- typeParams]
    forM_ fields $ \(fieldName, fieldType) ->
        modifyMS $ over ext (addField fieldName freshvars tapp fieldType)
    modifyMS $ over ext $ addTypeApp structName struct
    return struct

fixState :: Monad m => StateT s m a -> ReaderT s m a
fixState a = fst <$> (ReaderT $ runStateT a)

mutReader :: Monad m => ReaderT s m a -> StateT s m a
mutReader a =
    let read = runReaderT a
        fn s = do
            a <- read s
            return (a, s)
    in StateT fn

tforall :: [String] -> T -> T
tforall = TForall . S.fromList

basicTCEnv :: Bool -> MS (TCEnv TInfo) ()
basicTCEnv withTrace = do
    -- primitive
    let unit   = TNom $ RTPrim RTUnit
    let int    = TNom $ RTPrim RTInt
    let float  = TNom $ RTPrim RTFloat
    let str    = TNom $ RTPrim RTString
    let bool   = TNom $ RTPrim RTBool
    let sup    = TNom $ RTPrim RTAny
    let state  = TNom $ RTPrim RTState
    let tokens = TNom $ RTPrim RTTokens

    tokenTy' <- declareStruct "token" [] [
            (tokenId,   int),
            (tokenCol,  int),
            (tokenLine, int),
            (tokenVal,  str),
            (tokenName, str)
        ]

    astTy' <- declareStruct "ast" [] []
    listT  <- declareStruct "linkedlist" ["a"] []

    let tokenTy  = tokenTy' :# unit
        tokensTy = tokens
        astTy    = astTy'  :# unit
        errTy    = int :* str
        -- | forall a. (a, a) -> bool
        cmpTy = tforall ["a"] $
            (TFresh "a" :* TFresh "a") :-> bool

    -- make a attribute 'offset' for 'tokens' that 'tokens.offset : int'
    modifyMS $ over ext $ addField tokenOff S.empty tokensTy int

    enterType dsl_eq_n cmpTy
    enterType dsl_neq_n cmpTy

    -- null  : forall a. a
    enterType dsl_null_n $ tforall ["a"] $ TFresh "a"
    -- | peekable : (token_array_view, int) -> bool
    enterType dsl_peekable_n $
        (tokensTy :* int) :-> bool

    -- | peek: (token_array_view, int) -> token
    enterType dsl_peek_n $
        (tokensTy :* int) :-> tokenTy
    -- | match_tk : (token_array, view, int) ->
    --      (bool, any) iff withTrace
    --      ast_type
    let dsl_match_tk_ret_ty
            | withTrace = bool :* sup
            | otherwise = astTy
    enterType dsl_match_tk_n $
        (tokensTy :* int) :-> dsl_match_tk_ret_ty
    -- tk_id : constant (string -> int)
    enterType dsl_s_to_i_n $ str :-> int
    -- (unsafe) reset : (token_array_view, int) -> unit
    enterType dsl_reset_n $
        (tokensTy :* int) :-> unit
    -- cons : forall a. (a, a list) -> a list
    enterType dsl_cons_n $
        let listA = listT :# (TFresh "a")
        in  tforall ["a"] $
                (TFresh "a" :* listA) :-> listA
    -- nil : forall a. a list
    enterType dsl_cons_n $ listT :# (TFresh "a")
    -- to_errs : any -> err list
    enterType dsl_to_errs_n $ sup :-> listT :# errTy
    -- to_res: any -> ast
    enterType dsl_to_res_n $ sup :-> astTy
    -- to_any: forall a. a -> any
    enterType dsl_to_any_n $
        tforall ["a"] $ TFresh "a" :-> sup
    -- mk_ast: forall a. (str, a) -> ast
    enterType dsl_mkast_n $
        tforall ["a"] $ (str :* TFresh "a") :-> astTy
    -- always_true : state -> bool
    enterType (MName "always_true") $ state :-> bool
    return ()


commit = do
    s <- getsMS $ view ext
    return (view kinds s, view types s)

reset (kinds', types') =
    modifyMS $  over (ext . kinds) (const kinds') .
                over (ext . kinds) (const types')

enterType n t = modifyMS $ over ext (addType n t)

typeOf n = do
    a <- getsMS $ (M.lookup n) . view (ext . types)
    case a of
        Just a -> return a
        _      -> error $ "local variable " ++ show n ++ " not found"


assert_ :: WFF Unif -> MS (TCEnv TInfo) ()
assert_ cond = modifyMS $ over (ext . constr) (cond:)

infixl 2 |==|
a |==| b = Atom $ Unif {lhs=a, rhs=b, neq=False}

infixl 2 |/=|
a |/=| b = Atom $ Unif {lhs=a, rhs=b, neq=True}

getPrim :: RTPrim -> T
getPrim t = TNom $ RTPrim t

tc :: Reimu a -> MS (TCEnv TInfo) (Reimu T)
tc bIR@InT {outT=base} = case base of
    RExtern n (Left t) m -> do
        enterType n t
        m <- tc m
        return InT {tag = t, outT=RExtern n t m}
    RExtern n (Right (freevars, xs)) m -> do
        t <- declareStruct n freevars xs
        m <- tc m
        return InT {tag = t, outT=RExtern n t m}
    RAssign n a -> do
        t <- typeOf n
        a <- tc a
        assert_ $ tag a |==| t
        return InT {tag = t, outT=RAssign n a}
    RDecl n a -> do
      t <- TVar <$> newTVar
      -- auto recursive
      enterType n t
      -- for resume
      ext' <- commit
      a    <- tc a
      -- resume scope
      reset ext'
      -- assert:  typeof n == typeof a
      assert_ $ t |==| tag a
      return $ InT {tag=t, outT=RDecl n a}
    RIf a b c -> do
        -- for resume
        ext' <- commit

        -- if a, typeof a == bool
        cond <- tc a
        let bool = getPrim RTBool
        assert_ $ tag cond |==| bool

        ext'' <- commit

        tCls <- tc b
        reset ext''

        fCls <- tc c
        reset ext'
        t <- TVar <$> newTVar
        assert_ $ tag fCls |==| t
        assert_ $ tag tCls |==| t
        return $ InT {tag=t, outT=RIf cond tCls fCls}
    RCall f args -> do
        f    <- tc f
        args <- mapM tc args
        let unitT = getPrim RTUnit
        let argType = nTupleST $ map tag args
        paramType <- TVar <$> newTVar
        retType   <- TVar <$> newTVar
        assert_ $ tag f |==| paramType :-> retType
        assert_ $ argType |==| paramType
        return $ InT {tag=retType, outT=RCall f args}
    RAttr a attr -> do
        a <- tc a
        xs <- getsMS $ M.lookup attr . view (ext . fields)
        let ms =
              case xs of
                Nothing -> error $ "no struct has field " ++ attr
                Just xs -> flip map xs $ \(freshvars, tapp, tfield) -> do
                    pairs <- mapM freepair $ S.toList freshvars
                    let freemap = M.fromList pairs
                        tapp'   = free freemap tapp
                        tfield' = free freemap tfield
                    assert_ $ tag a |==| tapp'
                    return tfield'
            alts :: MS (TCEnv TInfo) T
            alts = case ms of
                []   -> error $ "impossible, attr list sized 0(" ++ attr ++ ")"
                x:xs -> foldl (<|>) x xs
        tfield <- alts
        return InT {tag = tfield, outT=RAttr a attr}
    RPrj a dim -> do
        a <- tc a
        let unitT = getPrim RTUnit
        mtd    <- getsMS $ maxTupleDim . view ext
        elty   <- TVar <$> newTVar
        initTp <- replicateM (dim-1) $ TVar <$> newTVar
        let ms = flip map [0 .. mtd - dim - 1] $ \i -> do
                   tailTp' <- replicateM i (TVar <$> newTVar)
                   let tailTp = nTupleST tailTp'
                       rStart
                         | tailTp == unitT = elty
                         | otherwise = elty :* tailTp
                   return $ foldr (:*) rStart initTp
            alts =  case ms of
                [] -> error $
                    "max tuple dim " ++ show mtd ++
                    ", but got projection on dim " ++ show dim
                x:xs -> foldl (<|>) x xs
        tp <- alts
        assert_ $ tag a |==| tp
        return InT {tag=elty, outT=RPrj a dim}
    RWhile a b -> do
        let unitT = getPrim RTUnit
        ext' <- commit
        cond <- tc a
        let bool = getPrim RTBool
        assert_ $ tag cond |==| bool
        body <- tc b
        reset ext'
        return InT {tag = unitT, outT = RWhile cond body}
    RSwitch a bs c -> do
        ext' <- commit
        test <- tc a
        ext'' <- commit
        -- can only switch on integer
        let int = getPrim RTInt
        assert_ $ int |==| tag test

        cases <- forM bs $ \(a, b) -> do
             case' <- tc a
            -- case i, i must be int
             assert_ $ int |==| tag case'
             body <- tc b
             reset ext''
             return (case', body)
        defau <- tc c
        t     <- TVar <$> newTVar
        forM_ (tag defau:map (tag . snd) cases) $ assert_ . (t |==|)
        reset ext'
        return InT {tag = t, outT = RSwitch test cases defau}
    RDef argNames b -> do
        ext'   <- commit
        tpArgs <- forM argNames $ \case
                MBuiltin "tokens" -> return $ getPrim RTTokens
                MBuiltin "state"  -> return $ getPrim RTState
                _                 -> (TVar <$> newTVar)
        let unitT = getPrim RTUnit
        let argType = nTupleST tpArgs
        retType <- TVar <$> newTVar
        let fType = argType :-> retType

        forM_ (zip argNames tpArgs) (uncurry enterType)
        body <- tc b
        assert_ $ tag body |==| retType
        reset ext'
        return InT {tag=fType, outT=RDef argNames body}

    RMutual names suite -> do
        xs <- forM names $ const (TVar <$> newTVar)
        forM (zip names xs) $ uncurry enterType
        let unitT = getPrim RTUnit
        suite <- mapM tc suite
        forM (zip xs suite) $ \(x, expr) ->
            assert_ $ x |==| tag expr

        let t = case suite of
                    [] -> unitT
                    xs -> tag $ last suite
        return InT {tag=t, outT=RMutual names suite}

    RVar n@(MBuiltin "tokens") -> do
        let tokensTy = getPrim RTTokens
        return InT {tag = tokensTy, outT = RVar n}

    RVar n@(MBuiltin "state") -> do
        let state = getPrim RTState
        return InT {tag = state, outT = RVar n}

    RVar n -> do
        t <- typeOf n
        return InT {tag=t, outT=RVar n}
    RInt i -> do
        let int  = getPrim RTInt
        return InT {tag=int, outT=RInt i}
    RStr s -> do
        let str  = getPrim RTString
        return InT {tag=str, outT=RStr s}
    RBool b -> do
        let bool = getPrim RTBool
        return InT {tag=bool, outT= RBool b}
    RTuple xs -> do
        xs <- mapM tc xs
        let unit = getPrim RTUnit
        let tp = nTupleST (map tag xs)
        return InT {tag=tp, outT=RTuple xs}
    RAnd a b -> do
        let bool = getPrim RTBool
        a <- tc a
        assert_ $ tag a |==| bool
        b <- tc b
        assert_ $ tag b |==| bool
        return  InT {tag = bool, outT = RAnd a b}
    ROr a b -> do
        let bool = getPrim RTBool
        a <- tc a
        assert_ $ tag a |==| bool
        b <- tc b
        assert_ $ tag b |==| bool
        return  InT {tag = bool, outT = ROr a b}


pruneTypedRIR :: Reimu T -> MS (TCEnv TInfo) (Reimu RT)
pruneTypedRIR InT {tag = t, outT = a} = do
    t <- prune t
    t <- toRT t
    a <- traverse pruneTypedRIR a
    return InT {tag = t, outT = a}

isSimpleExpr = \case
    RInt _ -> True
    RStr _ -> True
    RBool _ -> True
    RVar _ -> True
    RAttr _ _ -> True
    RCall _ _ -> True
    RPrj _ _ -> True
    RMutual ns xs -> L.length ns > 1 || all isSimpleExpr (map outT xs)
    RTuple xs -> all isSimpleExpr (map outT xs)
    RIf x y z -> all isSimpleExpr $ map outT [x, y, z]
    _ -> False