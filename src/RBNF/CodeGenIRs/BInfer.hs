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
    | BTSig String Int
    | BTVar String
    | BTGeneric [String] BT
    deriving (Eq, Ord)


instance Show BT where
    show = \case
        BTVar s -> s
        BTPrim s    -> show s
        BTFunc a b  -> showNest a ++ " -> " ++ show b
        BTTuple xs  -> "(" ++ L.intercalate "," (map show xs) ++ ")"
        BTGeneric xs t -> "forall " ++ (unwords xs) ++ ". " ++ show t
        BTApp t1 t2  -> show t1 ++ " " ++ showNest t2
        BTSig s _    -> s
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
    TVar i   -> error "unknown"
    TNom i   -> do
        let index = fromJust . L.find (\(a, b) -> b == i)
        getsMS $ fst . index . view (ext . prims)

addPrim :: BTPrim -> Int -> TInfo -> TInfo
addPrim t i = over prims (insert (BTPrim t) i)

addField :: String -> S.Set String -> T -> T -> TInfo -> TInfo
addField fieldName freshvars tapp fieldType =
        over fields (M.insertWith (++) fieldName [(freshvars, tapp, fieldType)])

addType :: AName -> T -> TInfo -> TInfo
addType tname t = over types (M.insert tname t)

addTypeApp :: AName -> T -> TInfo -> TInfo
addTypeApp tname t = over kinds (M.insert tname t)

addSig :: Int -> String -> TInfo -> TInfo
addSig i tname = over prims (insert (BTSig tname i) i)

nTupleST :: T -> [T] -> T
nTupleST unitT [] = unitT
nTupleST unitT (x:xs) =  foldl (:*) x xs

declareStruct :: String -> [String] -> [(String, T)] ->  MS (TCEnv TInfo) T
declareStruct structName typeParams fields = do
    let freshvars = S.fromList typeParams
    structTId  <- newTNom
    let tsig = TNom structTId
    unitT      <- getsMS $ TNom . (! (BTPrim BTUnit)) . view (ext . prims)
    let struct = TApp tsig $ nTupleST unitT [TFresh p | p <- typeParams]
    forM_ fields $ \(fieldName, fieldType) ->
        modifyMS $ over ext (addField fieldName freshvars struct fieldType)

    modifyMS $ over ext $ addTypeApp (AName structName) tsig
    modifyMS $ over ext $ addSig structTId structName
    return tsig

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
    unit  <- newTNom
    int   <- newTNom
    float <- newTNom
    str   <- newTNom
    bool  <- newTNom
    sup   <- newTNom
    state <- newTNom
    tokens <- newTNom
    modifyMS $ over ext $
        addPrim BTInt int .
        addPrim BTFloat float .
        addPrim BTUnit unit .
        addPrim BTString str .
        addPrim BTAny sup .
        addPrim BTBool bool .
        addPrim BTState state .
        addPrim BTTokens tokens

    tokenTy' <- declareStruct "token" [] [
            (tokenId,   TNom int),
            (tokenCol,  TNom int),
            (tokenLine, TNom int),
            (tokenVal,  TNom str),
            (tokenName, TNom str)
        ]

    astTy' <- declareStruct "ast" [] []
    listT  <- declareStruct "linkedlist" ["a"] []

    let tokenTy  = TApp tokenTy' $ TNom unit
        tokensTy = TNom tokens
        astTy    = TApp astTy'  $ TNom unit
        errTy    = TNom int :* TNom str
        -- | forall a. (a, a) -> bool
        cmpTy = tforall ["a"] $
            (TFresh "a" :* TFresh "a") :-> TNom bool

    -- make a attribute 'offset' for 'tokens' that 'tokens.offset : int'
    modifyMS $ over ext $ addField tokenOff S.empty tokensTy (TNom int)

    enterType dsl_eq_n cmpTy
    enterType dsl_neq_n cmpTy

    -- null  : forall a. a
    enterType dsl_null_n $ tforall ["a"] $ TFresh "a"
    -- | peekable : (token_array_view, int) -> bool
    enterType dsl_peekable_n $
        (tokensTy :* TNom int) :-> TNom bool

    -- | peek: (token_array_view, int) -> token
    enterType dsl_peek_n $
        (tokensTy :* TNom int) :-> tokenTy
    -- | match_tk : (token_array, view, int) ->
    --      (bool, any) iff withTrace
    --      ast_type
    let dsl_match_tk_ret_ty
            | withTrace = TNom bool :* TNom sup
            | otherwise = astTy
    enterType dsl_match_tk_n $
        (tokensTy :* TNom int) :-> dsl_match_tk_ret_ty
    -- tk_id : constant (string -> int)
    enterType dsl_s_to_i_n $
        TNom str :-> TNom int
    -- (unsafe) reset : (token_array_view, int) -> unit
    enterType dsl_reset_n $
        (tokensTy :* TNom int) :-> TNom unit
    -- cons : forall a. (a, a list) -> a list
    enterType dsl_cons_n $
        let listA = TApp listT (TFresh "a")
        in  tforall ["a"] $
                (TFresh "a" :* listA) :-> listA
    -- nil : forall a. a list
    enterType dsl_cons_n $ TApp listT (TFresh "a")
    -- to_errs : any -> err list
    enterType dsl_to_errs_n $
        TNom sup :-> TApp listT errTy
    -- to_res: any -> ast
    enterType dsl_to_res_n $
        TNom sup :-> astTy
    -- to_any: forall a. a -> any
    enterType dsl_to_any_n $
        tforall ["a"] $ TFresh "a" :-> TNom sup
    -- mk_ast: forall a. (str, a) -> ast
    enterType dsl_mkast_n $
        tforall ["a"] $ (TNom str :* TFresh "a") :-> astTy
    -- always_true : state -> bool
    enterType (AName "always_true") $
        TNom state :-> TNom bool

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

getPrim :: BTPrim -> MS (TCEnv TInfo) T
getPrim t = getsMS $ TNom . (! (BTPrim t)) . view (ext . prims)

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
        bool <- getPrim BTBool
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
        unitT <- getPrim BTUnit
        let argType = nTupleST unitT $ map tag args
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
        unitT  <- getPrim BTUnit
        mtd    <- getsMS $ maxTupleDim . view ext
        elty   <- TVar <$> newTVar
        initTp <- replicateM (dim-1) $ TVar <$> newTVar
        let ms = flip map [0 .. mtd - dim - 1] $ \i -> do
                   tailTp' <- replicateM i (TVar <$> newTVar)
                   let tailTp = nTupleST unitT tailTp'
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
        unitT <- getPrim BTUnit
        ext' <- commit
        cond <- tc a
        bool <- getPrim BTBool
        assert_ $ tag cond |==| bool
        body <- tc b
        reset ext'
        return InT {tag = unitT, outT = BWhile cond body}
    BSwitch a bs c -> do
        ext' <- commit
        test <- tc a
        ext'' <- commit
        -- can only switch on integer
        int  <- getPrim BTInt
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
        return InT {tag = tag defau, outT = BSwitch test cases defau}
    BDef fnName argNames b -> do
        ext'   <- commit
        tpArgs <- forM argNames $ \case
                ABuiltin "tokens" -> getPrim BTTokens
                ABuiltin "state"  -> getPrim BTState
                _                 -> (TVar <$> newTVar)
        unitT  <- getPrim BTUnit
        let argType = nTupleST unitT tpArgs
        retType <- TVar <$> newTVar
        let fType = argType :-> retType

        enterType fnName fType
        forM_ (zip argNames tpArgs) (uncurry enterType)
        body <- tc b
        assert_ $ tag body |==| retType
        reset ext'
        return InT {tag=fType, outT=BDef fnName argNames body}
    BBlock suite -> do
        unitT  <- getPrim BTUnit
        suite <- mapM tc suite
        let t = case suite of
                [] -> unitT
                xs -> tag $ last suite
        return InT {tag=t, outT=BBlock suite}
    BVar n@(ABuiltin "tokens") -> do
        tokensTy  <- getPrim BTTokens
        return InT {tag = tokensTy, outT = BVar n}

    BVar n@(ABuiltin "state") -> do
        state  <- getPrim BTState
        return InT {tag = state, outT = BVar n}

    BVar n -> do
        t <- typeOf n
        return InT {tag=t, outT=BVar n}
    BInt i -> do
        int  <- getPrim BTInt
        return InT {tag=int, outT=BInt i}
    BStr s -> do
        str  <- getPrim BTString
        return InT {tag=str, outT=BStr s}
    BBool b -> do
        bool  <- getPrim BTBool
        return InT {tag=bool, outT= BBool b}
    BTuple xs -> do
        xs <- mapM tc xs
        unit  <- getPrim BTUnit
        let tp = nTupleST unit (map tag xs)
        return InT {tag=tp, outT=BTuple xs}
    BAnd a b -> do
        bool <- getPrim BTBool
        a <- tc a
        assert_ $ tag a |==| bool
        b <- tc b
        assert_ $ tag b |==| bool
        return  InT {tag = bool, outT = BAnd a b}
    BOr a b -> do
        bool <- getPrim BTBool
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
            BDecl n InT {outT = BBlock codes} ->
                nest 4 $ sep $ pretty ("var " ++ show n ++ " :" ++ ts ++ " ="): map frec codes
            BDecl n code -> pretty ("var " ++ show n ++ " :" ++ ts ++ " = ") <+> frec code
            BAssign n InT {outT = BBlock codes} ->
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
            BDef fname args body ->
                let fnName = viaShow fname
                    argDef =  sep $ punctuate comma $ map viaShow args

                in  nest 4 $
                    vsep [
                        align $ vsep [
                            pretty ("[" ++ ts ++ "]")
                          , pretty "def" <+> fnName <> parens argDef
                        ]
                      , align $ nest 4 $ frec body
                    ]
            BBlock suite ->
                vsep $ map frec suite
            BVar n -> viaShow n
            BInt i -> viaShow i
            BStr s -> viaShow s
            BTuple elts ->
                pretty ("[" ++ ts ++ "]") <> pretty "tuple" <> tupled (map frec elts)
            BAnd a b -> frec a <+> pretty "and" <+> frec b
            BOr a b  -> frec a <+> pretty "or" <+> frec b