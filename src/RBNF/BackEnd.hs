{-# LANGUAGE LambdaCase #-}

module RBNF.BackEnd (ACode(..), AName(..), codeToString) where
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)

data AName = AName String | ABuiltin String
  deriving (Eq, Ord)

instance Show AName where
    show = \case
        AName s -> s
        ABuiltin s -> "%" ++ s

data ACode
    = AAssign AName ACode -- work in whole fn scope
    | ACall ACode [ACode]
    | AAttr ACode String
    | APrj  ACode Int
    | AIf ACode ACode ACode
    | AWhile ACode ACode
    | ASwitch ACode [(Int, ACode)] (Maybe ACode)
    | ADef AName [AName] ACode
    | ABlock [ACode]
    -- literal
    | AVar AName
    | AInt Integer
    | AStr String
    | ATuple [ACode]
    | AAnd ACode ACode
    | AOr  ACode ACode
    deriving (Eq, Ord)

codeToDoc = align . \case
    AAssign n (ABlock codes) ->
        nest 4 $ sep $ pretty (show n ++ " ="): map codeToDoc codes
    AAssign n code -> pretty (show n ++ " = ") <+> codeToDoc code
    ACall   f args -> codeToDoc f <> (parens . sep . punctuate comma $ map codeToDoc args)
    AAttr val attr -> codeToDoc val <> pretty ("." ++ attr)
    APrj val dim   -> codeToDoc val <> brackets (viaShow dim)
    AIf cond br1 br2 ->
        vsep [
            pretty "if" <+> nest 4 (codeToDoc cond)
          , pretty "then" <+> nest 4 (codeToDoc br1)
          , pretty "else" <+> nest 4 (codeToDoc br2)
        ]
    AWhile cond br ->
        nest 4 $
        vsep [
            pretty "while" <+> codeToDoc cond
          , codeToDoc br
        ]
    ASwitch expr cases default' ->
        vsep [
             pretty "switch" <+> codeToDoc expr
           , nest 4 $ align $ vsep [
               pretty "case" <+> viaShow i <+>
               pretty ":" <+>
               nest 4 (codeToDoc case')
               | (i, case') <- cases
             ]
           , case default' of
               Just default' -> pretty "default :" <+> nest 4 (codeToDoc default')
               _             -> emptyDoc
        ]
    ADef fname args body ->
        let fnName = viaShow fname
            argDef =  sep $ punctuate comma $ map viaShow args

        in  nest 4 $
            vsep [
                pretty "def" <+> fnName <> parens argDef
              , align $ nest 4 $ codeToDoc body
            ]
    ABlock suite ->
        vsep $ map codeToDoc suite
    AVar n -> viaShow n
    AInt i -> viaShow i
    AStr s -> viaShow s
    ATuple elts -> pretty "tuple" <> tupled (map codeToDoc elts)
    AAnd a b -> codeToDoc a <+> pretty "and" <+> codeToDoc b
    AOr a b -> codeToDoc a <+> pretty "or" <+> codeToDoc b

codeToString i = putDocW i . codeToDoc