-- | Type inference for BIR
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-06
-- License: BSD-3-clause
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}


module RBNF.CodeGenIRs.BInfer where


import RBNF.CodeGenIRs.ABuiltins
import RBNF.CodeGenIRs.B
import RBNF.CodeGenIRs.A (AName(..))
import RBNF.CodeGenIRs.HM
import RSolve.MapLike
import RSolve.PropLogic
import RSolve.MultiState
import RBNF.Utils

import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Applicative ((<|>))

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Data.Maybe (fromJust)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

type DNF = S.Set Unif
-- All here are reference types!

data BTPrim
    = BTInt
    | BTFloat
    | BTUnit
    | BTString
    | BTAny
    | BTBool
    -- blackboxed builtins
    | BTState
    | BTTokens
    deriving (Eq, Ord)

instance Show BTPrim where
    show = \case
        BTInt -> "int"
        BTFloat -> "float"
        BTUnit -> "()"
        BTString -> "str"
        BTAny -> "any"
        BTBool -> "bool"
        BTState -> "State"
        BTTokens -> "Tokens"

-- instance Show BTPrim wh
data BT
    = BTPrim BTPrim
    | BTTuple [BT]
    | BTFunc BT BT
    | BTApp BT BT
    | BTSig String
    | BTVar String
    | BTGeneric [String] BT
    deriving (Eq, Ord)

type T = HMT BT
type TCEnv = HMTCEnv BT
type Unif = HMUnif BT

instance Show BT where
    show = \case
        BTVar s -> s
        BTPrim s    -> show s
        BTFunc a b  -> showNest a ++ " -> " ++ show b
        BTTuple xs  -> "(" ++ L.intercalate "," (map show xs) ++ ")"
        BTGeneric xs t -> "forall " ++ (unwords xs) ++ ". " ++ show t
        BTApp t1 t2    -> show t1 ++ " " ++ showNest t2
        BTSig s        -> s
        where
            showNest s
                | isNest s  = "(" ++ show s ++ ")"
                | otherwise = show s
            isNest s = case s of
                BTFunc _ _    -> True
                BTGeneric _ _ -> True
                _             -> False
-- | Each module will hold a 'TInfo'
data TInfo
    = TInfo {
          _prims      :: [(BT, Int)]
          -- | a map that maps a field name to
          -- candicate lists of (structTypec, fieldType)
        , _fields     :: Map String [(S.Set String, T, T)]
        , _constr     :: [WFF Unif] -- _constraints
        , maxTupleDim :: Int
        -- context sensitive
        , _types  :: Map AName T
        , _kinds  :: Map AName T
       }

emptyTInfo = TInfo [] M.empty [] 6 M.empty M.empty

makeLenses ''TInfo


toBT :: T -> MS (TCEnv TInfo) BT
toBT = \case
    TApp a b -> BTApp <$> toBT a <*> toBT b
    a :* b   -> do
        hd <- toBT a
        let packTl = \case
                b1 :* b2 -> do
                    b1 <- toBT b1
                    (b1:) <$> packTl b2
                a -> do
                    a <- toBT a
                    case a of
                        BTPrim BTUnit -> return []
                        a -> return [a]
        tl <-  packTl b
        return $ BTTuple $ hd:tl
    a :-> b -> BTFunc <$> toBT a <*> toBT b
    TForall xs t -> BTGeneric (S.toList xs) <$> toBT t
    TFresh a -> return $ BTVar a
    TVar i   -> return $ BTVar ("@" ++ show i)
    TNom i   -> return i

addPrim :: BTPrim -> Int -> TInfo -> TInfo
addPrim t i = over prims (insert (BTPrim t) i)

addField :: String -> S.Set String -> T -> T -> TInfo -> TInfo
addField fieldName freshvars tapp fieldType =
        over fields (M.insertWith (++) fieldName [(freshvars, tapp, fieldType)])

addType :: AName -> T -> TInfo -> TInfo
addType tname t = over types (M.insert tname t)

addTypeApp :: AName -> T -> TInfo -> TInfo
addTypeApp tname t = over kinds (M.insert tname t)

nTupleST :: [T] -> T
nTupleST [] = getPrim BTUnit
nTupleST (x:xs) =  foldl (:*) x xs

declareStruct :: String -> [String] -> [(String, T)] ->  MS (TCEnv TInfo) T
declareStruct structName typeParams fields = do
    let freshvars = S.fromList typeParams
        struct  = TNom $ BTSig structName
        tapp = TApp struct $ nTupleST [TFresh p | p <- typeParams]
    forM_ fields $ \(fieldName, fieldType) ->
        modifyMS $ over ext (addField fieldName freshvars tapp fieldType)

    modifyMS $ over ext $ addTypeApp (AName structName) struct
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
    let unit   = TNom $ BTPrim BTUnit
    let int    = TNom $ BTPrim BTInt
    let float  = TNom $ BTPrim BTFloat
    let str    = TNom $ BTPrim BTString
    let bool   = TNom $ BTPrim BTBool
    let sup    = TNom $ BTPrim BTAny
    let state  = TNom $ BTPrim BTState
    let tokens = TNom $ BTPrim BTTokens

    tokenTy' <- declareStruct "token" [] [
            (tokenId,   int),
            (tokenCol,  int),
            (tokenLine, int),
            (tokenVal,  str),
            (tokenName, str)
        ]

    astTy' <- declareStruct "ast" [] []
    listT  <- declareStruct "linkedlist" ["a"] []

    let tokenTy  = TApp tokenTy' $ unit
        tokensTy = tokens
        astTy    = TApp astTy'  $ unit
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
        let listA = TApp listT (TFresh "a")
        in  tforall ["a"] $
                (TFresh "a" :* listA) :-> listA
    -- nil : forall a. a list
    enterType dsl_cons_n $ TApp listT (TFresh "a")
    -- to_errs : any -> err list
    enterType dsl_to_errs_n $ sup :-> TApp listT errTy
    -- to_res: any -> ast
    enterType dsl_to_res_n $ sup :-> astTy
    -- to_any: forall a. a -> any
    enterType dsl_to_any_n $
        tforall ["a"] $ TFresh "a" :-> sup
    -- mk_ast: forall a. (str, a) -> ast
    enterType dsl_mkast_n $
        tforall ["a"] $ (str :* TFresh "a") :-> astTy
    -- always_true : state -> bool
    enterType (AName "always_true") $ state :-> bool
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

getPrim :: BTPrim -> T
getPrim t = TNom $ BTPrim t

tc :: BIR a -> MS (TCEnv TInfo) (BIR T)
tc bIR@InT {outT=base} = case base of
    BAssign n a -> do
        t <- typeOf n
        a <- tc a
        assert_ $ tag a |==| t
        return $ InT {tag = t, outT=BAssign n a}
    BDecl n a -> do
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
      return $ InT {tag=t, outT=BDecl n a}
    BIf a b c -> do
        -- for resume
        ext' <- commit

        -- if a, typeof a == bool
        cond <- tc a
        let bool = getPrim BTBool
        assert_ $ tag cond |==| bool

        ext'' <- commit

        tCls <- tc b
        reset ext''

        fCls <- tc c
        reset ext'
        t <- TVar <$> newTVar
        assert_ $ tag fCls |==| t
        assert_ $ tag tCls |==| t
        return $ InT {tag=t, outT=BIf cond tCls fCls}
    BCall f args -> do
        f    <- tc f
        args <- mapM tc args
        let unitT = getPrim BTUnit
        let argType = nTupleST $ map tag args
        paramType <- TVar <$> newTVar
        retType   <- TVar <$> newTVar
        assert_ $ tag f |==| paramType :-> retType
        assert_ $ argType |==| paramType
        return $ InT {tag=retType, outT=BCall f args}
    BAttr a attr -> do
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
        return InT {tag = tfield, outT=BAttr a attr}
    BPrj a dim -> do
        a <- tc a
        let unitT = getPrim BTUnit
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
        return InT {tag=elty, outT=BPrj a dim}
    BWhile a b -> do
        let unitT = getPrim BTUnit
        ext' <- commit
        cond <- tc a
        let bool = getPrim BTBool
        assert_ $ tag cond |==| bool
        body <- tc b
        reset ext'
        return InT {tag = unitT, outT = BWhile cond body}
    BSwitch a bs c -> do
        ext' <- commit
        test <- tc a
        ext'' <- commit
        -- can only switch on integer
        let int = getPrim BTInt
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
        return InT {tag = t, outT = BSwitch test cases defau}
    BDef argNames b -> do
        ext'   <- commit
        tpArgs <- forM argNames $ \case
                ABuiltin "tokens" -> return $ getPrim BTTokens
                ABuiltin "state"  -> return $ getPrim BTState
                _                 -> (TVar <$> newTVar)
        let unitT = getPrim BTUnit
        let argType = nTupleST tpArgs
        retType <- TVar <$> newTVar
        let fType = argType :-> retType

        forM_ (zip argNames tpArgs) (uncurry enterType)
        body <- tc b
        assert_ $ tag body |==| retType
        reset ext'
        return InT {tag=fType, outT=BDef argNames body}

    BMutual names suite -> do
        xs <- forM names $ const (TVar <$> newTVar)
        forM (zip names xs) $ uncurry enterType
        let unitT = getPrim BTUnit
        suite <- mapM tc suite
        forM (zip xs suite) $ \(x, expr) ->
            assert_ $ x |==| tag expr

        let t = case suite of
                    [] -> unitT
                    xs -> tag $ last suite
        return InT {tag=t, outT=BMutual names suite}

    BVar n@(ABuiltin "tokens") -> do
        let tokensTy = getPrim BTTokens
        return InT {tag = tokensTy, outT = BVar n}

    BVar n@(ABuiltin "state") -> do
        let state = getPrim BTState
        return InT {tag = state, outT = BVar n}

    BVar n -> do
        t <- typeOf n
        return InT {tag=t, outT=BVar n}
    BInt i -> do
        let int  = getPrim BTInt
        return InT {tag=int, outT=BInt i}
    BStr s -> do
        let str  = getPrim BTString
        return InT {tag=str, outT=BStr s}
    BBool b -> do
        let bool = getPrim BTBool
        return InT {tag=bool, outT= BBool b}
    BTuple xs -> do
        xs <- mapM tc xs
        let unit = getPrim BTUnit
        let tp = nTupleST (map tag xs)
        return InT {tag=tp, outT=BTuple xs}
    BAnd a b -> do
        let bool = getPrim BTBool
        a <- tc a
        assert_ $ tag a |==| bool
        b <- tc b
        assert_ $ tag b |==| bool
        return  InT {tag = bool, outT = BAnd a b}
    BOr a b -> do
        let bool = getPrim BTBool
        a <- tc a
        assert_ $ tag a |==| bool
        b <- tc b
        assert_ $ tag b |==| bool
        return  InT {tag = bool, outT = BOr a b}


pruneTypedBIR :: BIR T -> MS (TCEnv TInfo) (BIR BT)
pruneTypedBIR InT {tag = t, outT = a} = do
    t <- prune t
    t <- toBT t
    a <- traverse pruneTypedBIR a
    return InT {tag = t, outT = a}

typedBIRToDoc :: BIR BT -> Doc ann
typedBIRToDoc = frec
    where
        frec InT {tag=t, outT=base} =
            let ts = show t in align $ case base of
            BDecl n InT {outT = BMutual [] codes} ->
                nest 4 $ sep $ pretty ("var " ++ show n ++ " :" ++ ts ++ " ="): map frec codes
            BDecl n code -> pretty ("var " ++ show n ++ " :" ++ ts ++ " = ") <+> frec code
            BAssign n InT {outT = BMutual [] codes} ->
                nest 4 $ sep $ pretty (show n ++ " :" ++ ts ++ " ="): map frec codes
            BAssign n code -> pretty (show n ++ " :" ++ ts ++ " = ") <+> frec code
            BCall   f args -> vsep [
                    pretty ("[" ++ ts ++ "]"),
                    frec f <> (parens . sep . punctuate comma $ map frec args)
                ]
            BAttr val attr -> pretty ("[" ++ ts ++ "]") <>
                frec val <> pretty ("." ++ attr)
            BPrj val dim   -> pretty ("[" ++ ts ++ "]") <>
                frec val <> brackets (viaShow dim)
            BIf cond br1 br2 ->
                vsep [
                  pretty ("[" ++ ts ++ "]") <>  pretty "if"   <+> nest 4 (frec cond)
                , pretty "then" <+> nest 4 (frec br1)
                , pretty "else" <+> nest 4 (frec br2)
                ]
            BWhile cond br ->
                nest 4 $
                vsep [
                  pretty ("[" ++ ts ++ "]") <> pretty "while" <+> frec cond
                , frec br
                ]
            BSwitch expr cases default' ->
                vsep [
                  pretty ("[" ++ ts ++ "]") <> pretty "switch" <+> frec expr
                , nest 4 $ align $ vsep [
                    pretty "case" <+> frec i <+>
                    pretty ":" <+>
                    nest 4 (frec case')
                    | (i, case') <- cases
                    ]
                , pretty "default :" <+> nest 4 (frec default')
                ]
            BDef args body ->
                let argDef =  sep $ punctuate comma $ map viaShow args

                in  nest 4 $
                    vsep [
                        align $ vsep [
                            pretty ("[" ++ ts ++ "]")
                          , parens argDef <+> pretty "->"
                        ]
                      , align $ nest 4 $ frec body
                    ]
            BVar n -> viaShow n
            BInt i -> viaShow i
            BStr s -> viaShow s
            BBool b -> viaShow b
            BTuple elts ->
                pretty ("[" ++ ts ++ "]") <> pretty "tuple" <> tupled (map frec elts)

            BMutual names suite ->
                let n         = length names
                    (l1, l2)  = L.splitAt n suite
                    recursive = zipWith (\name v -> pretty ("rec " ++ show name ++ " =") <+> frec v)
                                        names l1
                in vsep $ recursive ++ map frec l2
            BAnd a b -> frec a <+> pretty "and" <+> frec b
            BOr a b  -> frec a <+> pretty "or" <+> frec b