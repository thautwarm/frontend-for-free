-- | Based on IR A, making it extensible
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-07-13
-- License: BSD-3-clause
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module RBNF.CodeGenIRs.B where
import RBNF.CodeGenIRs.A
import GHC.Generics
import Control.Monad.State
import Data.Functor

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L


import Debug.Trace

data BBase a
    = BAssign AName a
    | BDecl AName a
    | BCall a [a]
    | BAttr a String
    | BPrj  a Int
    | BIf a a a
    | BWhile a a
    | BSwitch a [(a, a)] a
    | BDef [AName] a
    | BMutual [AName] [a]
    -- literal
    | BVar AName
    | BInt Integer
    | BStr String
    | BBool Bool
    | BTuple [a]
    | BAnd a a
    | BOr  a a
    deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

data FixT f t = InT {tag :: t, outT :: f (FixT f t)}
deriving instance Show b => Show (FixT BBase b)
deriving instance Eq b => Eq (FixT BBase b)
deriving instance Ord b => Ord (FixT BBase b)
deriving instance Functor f => Functor (FixT f)
deriving instance Foldable f => Foldable (FixT f)
deriving instance Traversable f => Traversable (FixT f)


inc :: State Int Int
inc = do
    i <- get
    put $ i + 1
    return i

type BIR a = FixT BBase a


aToB :: AIR -> State Int (FixT BBase Int)
aToB = (InT <$> inc <*>) . f
    where
      f = \case
        AAssign n ir -> BAssign n <$> aToB ir
        ACall f args ->
            BCall <$> aToB f <*> mapM aToB args
        AAttr val attr ->
            BAttr <$> aToB val <*> pure attr
        APrj val dim ->
            BPrj <$> aToB val <*> pure dim
        AIf cond cla1 cla2 ->
            BIf <$> aToB cond <*> aToB cla1 <*> aToB cla2
        AWhile cond body ->
            BWhile <$> aToB cond <*> aToB body
        ADef _ args body -> BDef args <$> aToB body
        ASwitch val cases defau ->
            let cases' =
                    forM cases $ \(i, body) ->
                    aToB body >>= \body ->
                    aToB i    >>= \i    -> return (i, body)
            in BSwitch <$> aToB val <*> cases' <*> aToB defau
        ABlock  suite -> do
          let group defs bs = \case
                [] -> (reverse defs, reverse bs)
                def@(ADef n args body):xs -> group ((n, def):defs) bs xs
                x:xs                -> group defs (x:bs) xs
              (defs, bs) = group [] [] suite
              names = map fst defs
          defs <- mapM (aToB . snd) defs
          bs   <- mapM aToB bs
          let suite' = defs ++ bs
          return $ BMutual names suite'
        AVar  n      -> return $ BVar n
        AInt  i      -> return $ BInt i
        AStr  s      -> return $ BStr s
        ABool b      -> return $ BBool b
        ATuple elts  -> BTuple <$> mapM aToB elts
        AAnd a b     -> BAnd <$> aToB a <*> aToB b
        AOr a b      -> BOr <$> aToB a <*> aToB b

-- | AIR involves no declarations for local variables.
--   We make 'resolveDecl' to do so.
resolveDecl :: BIR a -> State (S.Set AName) (BIR a)
-- | some sketches
-- t = BBase, f = State (S.Set VName)
-- a = BIR _
-- f (t a) -> State (S.Set Name) (BIR a)
-- rslDcl :: BIR a -> State (S.Set VName) BIR a
--  a <- traverse f (outT rsl)
-- {outT = InT a}

resolveDecl bIR@InT {outT=base} =
  case base of
    BAssign n a ->
      gets (n `S.member`) >>= \case
        False -> do
          modify (S.insert n)
          resumeTag <$> (BDecl n <$> resolveDecl a)
        True  -> resumeTag <$> genericVisit
    BIf a b c -> do
      s <- get
      let (cond, s') = flip runState s  $ resolveDecl a
          tCls  = flip evalState s' $ resolveDecl b
          fCls  = flip evalState s' $ resolveDecl c
      return . resumeTag $ BIf cond tCls fCls

    BWhile a b -> do
      s <- get
      let (cond, s') = flip runState  s  $ resolveDecl a
          body       = flip evalState s' $ resolveDecl b
      return . resumeTag $ BWhile cond body
    BDef args b -> do
      s' <- get
      put $ S.fromList args
      ret <- genericVisit
      put s'
      return . resumeTag $ ret
    BSwitch a bs c -> do
      s <- get
      let (v, s') = flip runState s $ resolveDecl a
          cases   = [(case', flip evalState s' $ resolveDecl b) | (case', b) <- bs]
          defau   = flip evalState s' $ resolveDecl c
      return . resumeTag $ BSwitch v cases defau
    BMutual xs bs -> do
        s <- get
        modify $ S.union (S.fromList xs)
        base <- genericVisit
        let ret = resumeTag base
        put s
        return ret
    _ -> resumeTag <$> genericVisit

  where
    genericVisit = traverse resolveDecl base
    resumeTag  base = bIR {outT = base}

bIRToDoc InT {outT=base} = align $
  case base of
    BDecl n InT {outT = BMutual [] codes} ->
      nest 4 $ sep $ pretty ("var " ++ show n ++ " ="): map bIRToDoc codes
    BDecl n code -> pretty ("var " ++ show n ++ " = ") <+> bIRToDoc code
    BAssign n InT {outT = BMutual [] codes} ->
        nest 4 $ sep $ pretty (show n ++ " ="): map bIRToDoc codes
    BAssign n code -> pretty (show n ++ " = ") <+> bIRToDoc code
    BCall   f args -> bIRToDoc f <> (parens . sep . punctuate comma $ map bIRToDoc args)
    BAttr val attr -> bIRToDoc val <> pretty ("." ++ attr)
    BPrj val dim   -> bIRToDoc val <> brackets (viaShow dim)
    BIf cond br1 br2 ->
        vsep [
            pretty "if"   <+> nest 4 (bIRToDoc cond)
          , pretty "then" <+> nest 4 (bIRToDoc br1)
          , pretty "else" <+> nest 4 (bIRToDoc br2)
        ]
    BWhile cond br ->
        nest 4 $
        vsep [
            pretty "while" <+> bIRToDoc cond
          , bIRToDoc br
        ]
    BSwitch expr cases default' ->
        vsep [
             pretty "switch" <+> bIRToDoc expr
           , nest 4 $ align $ vsep [
               pretty "case" <+> bIRToDoc i <+>
               pretty ":" <+>
               nest 4 (bIRToDoc case')
               | (i, case') <- cases
             ]
           , pretty "default :" <+> nest 4 (bIRToDoc default')
        ]
    BDef args body ->
        let argDef =  sep $ punctuate comma $ map viaShow args

        in  nest 4 $
            vsep [
                parens argDef <+> pretty "->"
              , align $ nest 4 $ bIRToDoc body
            ]
    BMutual names suite ->
        let n         = length names
            (l1, l2)  = L.splitAt n suite
            recursive = zipWith (\name v -> pretty ("rec " ++ show name ++ " =") <+> bIRToDoc v)
                                names l1
        in vsep $ recursive ++ map bIRToDoc l2
    BVar n  -> viaShow n
    BInt i  -> viaShow i
    BStr s  -> viaShow s
    BBool b -> viaShow b
    BTuple elts -> pretty "tuple" <> tupled (map bIRToDoc elts)
    BAnd a b -> bIRToDoc a <+> pretty "and" <+> bIRToDoc b
    BOr a b -> bIRToDoc a <+> pretty "or" <+> bIRToDoc b

printBIR i = putDocW i . bIRToDoc