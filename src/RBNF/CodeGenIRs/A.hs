-- | Sufficient for dynamic languages to do codegen
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-03
-- License: BSD-3-clause
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module RBNF.CodeGenIRs.A (AIR(..), AName(..), printAIR) where
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import GHC.Generics

data AName = AName String | ABuiltin String
  deriving (Eq, Ord, Generic)

instance Show AName where
    show = \case
        AName s -> s
        ABuiltin s -> "%" ++ s

data AIR
    = AAssign AName AIR -- work in whole fn scope
    | ACall AIR [AIR]
    | AAttr AIR String
    | APrj  AIR Int
    | AIf AIR AIR AIR
    | AWhile AIR AIR
    | ASwitch AIR [(AIR, AIR)] AIR
    | ADef AName [AName] AIR
    | ABlock [AIR]
    -- literal
    | AVar AName
    | AInt Integer
    | AStr String
    | ABool Bool
    | ATuple [AIR]
    | AAnd AIR AIR
    | AOr  AIR AIR
    deriving (Eq, Ord, Generic)

isSimpleExpr = \case
    AInt _ -> True
    AStr _ -> True
    ABool _ -> True
    AVar _ -> True
    AAttr _ _ -> True
    ACall _ _ -> True
    APrj _ _ -> True
    ATuple xs -> all isSimpleExpr xs
    AIf x y z ->    isSimpleExpr x
                 && isSimpleExpr y
                 && isSimpleExpr z
    _ -> False

aIRToDoc = align . \case
    AAssign n (ABlock codes) ->
        nest 4 $ sep $ pretty (show n ++ " ="): map aIRToDoc codes
    AAssign n code@(isSimpleExpr -> False) ->
        nest 4 $ vsep [ pretty (show n ++ " = "), aIRToDoc code]
    AAssign n code -> pretty (show n ++ " = ") <+> aIRToDoc code
    ACall   f args -> aIRToDoc f <> (parens . sep . punctuate comma $ map aIRToDoc args)
    AAttr val attr -> aIRToDoc val <> pretty ("." ++ attr)
    APrj val dim   -> aIRToDoc val <> brackets (viaShow dim)
    AIf cond br1 br2 ->
        vsep [
            pretty "if" <+> nest 4 (aIRToDoc cond)
          , pretty "then" <+> nest 4 (aIRToDoc br1)
          , pretty "else" <+> nest 4 (aIRToDoc br2)
        ]
    AWhile cond br ->
        nest 4 $
        vsep [
            pretty "while" <+> aIRToDoc cond
          , aIRToDoc br
        ]
    ASwitch expr cases default' ->
        vsep [
             pretty "switch" <+> aIRToDoc expr
           , nest 2 $ align $ vsep [
               nest 2 $ vsep [
                    pretty "case" <+> (aIRToDoc i) <+> pretty ":"
                  , aIRToDoc case'
               ] | (i, case') <- cases
             ]
           , pretty "default :" <+> nest 4 (aIRToDoc default')
        ]
    ADef fname args body ->
        let fnName = viaShow fname
            argDef =  sep $ punctuate comma $ map viaShow args

        in  nest 4 $
            vsep [
                pretty "def" <+> fnName <> parens argDef
              , align $ nest 4 $ aIRToDoc body
            ]
    ABlock suite ->
        vsep $ map aIRToDoc suite
    AVar n -> viaShow n
    AInt i -> viaShow i
    AStr s -> viaShow s
    ABool b -> viaShow b
    ATuple elts -> pretty "tuple" <> tupled (map aIRToDoc elts)
    AAnd a b -> aIRToDoc a <+> pretty "and" <+> aIRToDoc b
    AOr a b -> aIRToDoc a <+> pretty "or" <+> aIRToDoc b

printAIR i = putDocW i . aIRToDoc