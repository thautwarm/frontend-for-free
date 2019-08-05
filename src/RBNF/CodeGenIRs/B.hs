-- | Based on IR A, making it extensible
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-07-13
-- License: BSD-3-clause
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module RBNF.CodeGenIRs.B where
import RBNF.CodeGenIRs.A
import Control.Monad.State
import Control.Arrow

data BIR a
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
    deriving (Eq, Ord)

class BExt a b | a -> b where
    ext  :: a -> b
    bir   :: a -> BIR a

data ID b = ID {info :: b, ir :: BIR (ID b)}

inc :: State Int Int
inc = do
    i <- get
    put $ i + 1
    return i

aToB :: AIR -> State Int (ID Int)
aToB = (ID <$> inc <*>) . \case
    AAssign n ir ->
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