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
import RSolve.PropLogic
import RSolve.MultiState
import RBNF.Utils

import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Applicative ((<|>))

import qualified Data.Map as M
import qualified Data.Set as S

type DNF = S.Set Unif
-- All here are reference types!

data BTPrim
    = BTInt
    | BTFloat
    | BTUnit
    | BTString
    | BTAny
    | BTBool
    deriving (Eq, Ord)

data BT
    = BTPrim BTPrim
    | Tuple [BT]
    | BTFunc BT BT
    | BTStruct [(String, BT)]

-- | Each module will hold a 'TInfo'
data TInfo
    = TInfo {
          _prims  :: Map BTPrim Int
          -- | a map that maps a field name to
          -- candicate lists of (structTypec, fieldType)
        , _fields :: Map String [(S.Set String, T, T)]
        , _types  :: Map AName T
        , _kinds  :: Map AName T
        , _constr :: [WFF Unif] -- _constraints
        , maxTupleDim :: Int
       }

makeLenses ''TInfo

addPrim :: BTPrim -> Int -> TInfo -> TInfo
addPrim t i = over prims (M.insert t i)

addField :: String -> S.Set String -> T -> T -> TInfo -> TInfo
addField fieldName freshvars tapp fieldType =
        over fields (M.insertWith (++) fieldName [(freshvars, tapp, fieldType)])

addType :: AName -> T -> TInfo -> TInfo
addType tname t = over types (M.insert tname t)

addTypeApp :: AName -> T -> TInfo -> TInfo
addTypeApp tname t = over kinds (M.insert tname t)

-- zipS :: MS s1 () -> MS s2 () -> MS (s1, s2) ()
-- zipS m1 m2 =
--     let f1 = runMS m1 -- \s1 -> [((), s1)]
--         f2 = runMS m2
--     in MS $ \(s1, s2) -> zipWith (\((), s1') ((), s2') -> ((), (s1', s2'))) (f1 s1) (f2 s2)

nTupleST :: T -> [T] -> T
nTupleST unitT []     =  unitT
nTupleST unitT (x:xs) =  foldr (:*) x xs

declareStruct :: [String] -> [(String, T)] -> StateT TInfo (MS (TCEnv ext)) T
declareStruct typeParams fields = do
    let freshvars = S.fromList typeParams
    structTId  <- lift newTNom
    let tsig = TNom structTId
    unitT      <- gets $ TNom . (M.! BTUnit) . view prims
    let struct = TApp tsig $ nTupleST unitT [TFresh p | p <- typeParams]
    forM_ fields $ \(fieldName, fieldType) ->
        modify $ addField fieldName freshvars struct fieldType
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

basicTCEnv :: StateT TInfo (MS (TCEnv ctx)) ()
basicTCEnv = do
    unit  <- lift newTNom
    int   <- lift newTNom
    float <- lift newTNom
    str   <- lift newTNom
    bool  <- lift newTNom
    sup   <- lift newTNom
    modify $
        addPrim BTInt int .
        addPrim BTFloat float .
        addPrim BTUnit unit .
        addPrim BTString str .
        addPrim BTAny sup .
        addPrim BTBool bool
    tsig <- declareStruct [] [
            (tokenId,   TNom int),
            (tokenCol,  TNom int),
            (tokenLine, TNom int),
            (tokenVal,  TNom str),
            (tokenName, TNom str)
        ]
    modify $ addTypeApp (AName "token") tsig
    return ()

-- assignTVar :: BIR a -> ReaderT TInfo (MS (TCEnv ctx)) (BIR T)
-- assignTVar bIR@InT {outT=base} = do
--     base <- traverse assignTVar base
--     i    <- lift newTVar
--     return $ InT {tag = TVar i, outT = base}

reset ext' = modifyMS $ over ext (const ext')

enterType n t = modifyMS $ over ext (addType n t)

typeOf n = getsMS $ (M.! n) . view (ext . types)

assert_ :: WFF Unif -> MS (TCEnv TInfo) ()
assert_ cond = modifyMS $ over (ext . constr) (cond:)

infixl 2 |==|
a |==| b = Atom $ Unif {lhs=a, rhs=b, neq=False}

infixl 2 |/=|
a |/=| b = Atom $ Unif {lhs=a, rhs=b, neq=True}

getPrim :: BTPrim -> MS (TCEnv TInfo) T
getPrim t = getsMS $ TNom . (M.! t) . view (ext . prims)

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
      ext' <- getsMS $ view ext
      a    <- tc a
      -- resume scope
      reset ext'
      -- assert:  typeof n == typeof a
      assert_ $ t |==| tag a
      return $ InT {tag=t, outT=BDecl n a}
    BIf a b c -> do
        -- for resume
        ext' <- getsMS $ view ext

        -- if a, typeof a == bool
        cond <- tc a
        bool <- getPrim BTBool
        assert_ $ tag cond |==| bool

        ext'' <- getsMS $ view ext

        tCls <- tc b
        reset ext''

        fCls <- tc c
        reset ext'

        assert_ $ tag fCls |==| tag tCls
        return $ InT {tag=tag tCls, outT=BIf cond tCls fCls}
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
                   return $ foldr (:*) (elty :* tailTp) initTp
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
        ext' <- getsMS $ view ext
        cond <- tc a
        bool <- getPrim BTBool
        assert_ $ tag cond |==| bool
        body <- tc b
        reset ext'
        return InT {tag = unitT, outT = BWhile cond body}
    BSwitch a bs c -> do
        ext' <- getsMS $ view ext
        test <- tc a
        ext'' <- getsMS $ view ext
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
        forM_ cases $ assert_ . (tag defau |==|) . tag . snd
        reset ext'
        return InT {tag = tag defau, outT = BSwitch test cases defau}
    BDef fnName argNames b -> do
        ext'   <- getsMS $ view ext
        tpArgs <- forM argNames $ const (TVar <$> newTVar)
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
    BVar n -> do
        t <- typeOf n
        return InT {tag=t, outT=BVar n}
    BInt i -> do
        int  <- getPrim BTInt
        return InT {tag=int, outT=BInt i}
    BStr s -> do
        str  <- getPrim BTString
        return InT {tag=str, outT=BStr s}
    BTuple xs -> do
        xs <- mapM tc xs
        unit  <- getPrim BTUnit
        let tp = nTupleST unit (map tag xs)
        return InT {tag=tp, outT=BTuple xs}
    _ -> error ".." -- TODO
