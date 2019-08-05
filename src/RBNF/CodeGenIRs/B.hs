-- | Based on IR A, making it extensible
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-07-13
-- License: BSD-3-clause
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module RBNF.CodeGenIRs.B where
import RBNF.CodeGenIRs.A


import RSolve.HM

import GHC.Generics
import Control.Monad.State
import Control.Arrow
import Data.Functor

data BIR b a
    = BAssign AName a
    | BCall a [a]
    | BAttr a String
    | BPrj  a Int
    | BIf a a a
    | BWhile a a
    | BSwitch a [(a, a)] (Maybe a)
    | BDef AName [AName] a
    | BBlock [a]
    -- literal
    | BDecl AName
    | BVar AName
    | BInt Integer
    | BStr String
    | BTuple [a]
    | BAnd a a
    | BOr  a a
    | BTag b a
    deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

-- fix f a = f (fix f) a
-- FixT :: (* -> *) -> *

newtype FixT f = InT {outT :: f (FixT f)}

instance Show i => Show (FixT (BIR i)) where
    show (InT s) = "(" ++ show s ++ ")"

instance Eq i => Eq (FixT (BIR i)) where
    InT i == InT j = i == j

instance Ord i => Ord (FixT (BIR i)) where
    InT i <= InT j = i <= j

-- class BExt a b | a -> b where
--     ext  :: a -> b
--     bir   :: a -> BIR a

-- data a = BTag {tag :: a, ir :: BIR a}
--     deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

inc :: State Int Int
inc = do
    i <- get
    put $ i + 1
    return i

aToB :: AIR -> State Int (FixT (BIR Int))
aToB = f >=> return . InT
    where
      f = \case
        AAssign n ir -> do
            BAssign n <$> aToB ir
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
        ADef n args body -> BDef n args <$> aToB body
        ASwitch val cases defau ->
            let cases' =
                    forM cases $ \(i, body) ->
                    aToB body >>= \body ->
                    aToB i    >>= \i    -> return (i, body)
                defau' = case defau of
                    Just defau -> Just <$> aToB defau
                    _          -> return Nothing
            in BSwitch <$> aToB val <*> cases' <*> defau'
        ABlock suite -> BBlock <$> mapM aToB suite
        AVar  n      -> return $ BVar n
        AInt  i      -> return $ BInt i
        AStr  s      -> return $ BStr s
        ATuple elts  -> BTuple <$> mapM aToB elts
        AAnd a b     -> BAnd <$> aToB a <*> aToB b
        AOr a b      -> BOr <$> aToB a <*> aToB b