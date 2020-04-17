module RBNF.BackEnds.Julia where
import           Data.Text.Prettyprint.Doc
import           RBNF.IRs.Marisa
import           RBNF.Name
import           Control.Monad.State


showMN :: Name -> String
showMN n =
    let n' = show n in
    if isJavaName n' then n'
    else concat ["var\"", n', "\""]

genJl quoted = align . \case
    MKAssign n (MKBlock codes) -> vsep
        [ nest 4 $ sep $ pretty (showMN n ++ " = begin") : map (genJl True) codes
        , pretty "end"
        ]
    MKAssign n code@(isSimpleMarisa -> False) ->
        nest 4 $ vsep [pretty (showMN n ++ " = "), genJl False code]
    MKAssign n code -> pretty (showMN n ++ " = ") <+> genJl False code
    MKCall f args ->
        genJl False f <> (parens . sep . punctuate comma $ map (genJl False) args)
    MKAttr val attr   -> genJl False val <> pretty ("." ++ attr)
    MKPrj  val dim    -> genJl False val <> brackets (viaShow dim)
    MKIf cond br1 br2 -> vsep
        [ pretty "if" <+> nest 4 (genJl False cond)
        , indent 4 $ genJl True br1
        , pretty "else"
        , indent 4 $ genJl True br2
        , pretty "end"
        ]
    MKWhile cond br -> vsep
        [ pretty "while" <+> genJl False cond
        , indent 4 $ genJl True br
        , pretty "end"
        ]
    MKSwitch expr cases default' -> vsep
        [ pretty "@switch " <+> genJl False expr <+> pretty "begin"
        , nest 2 $ align $ vsep
            [ nest 2
                  $ vsep
                        [ pretty "@case" <+> (genJl False i)
                        , genJl True case'
                        ]
            | (i, case') <- cases
            ]
        , nest 2 $ align $ vsep
                [ pretty "@default"
                , nest 4 (genJl True default')
                ]
        ]
    MKDef fname args body ->
        let
            fnName = pretty $ showMN fname
            argDef = align . sep $ punctuate comma $ map (pretty . showMN) args
        in vsep
            [ pretty "function" <+> fnName <> parens argDef
            , indent 2 . align $ genJl True body
            , pretty "end"
            ]
    MKBlock suite  ->
        align . vsep $
        if quoted then
             map (genJl True) suite
        else [ nest 2 $ vsep $ pretty "begin" : map (genJl True) suite
             , pretty "end"
             ]
    MKVar   n      -> pretty $ showMN n
    MKInt   i      -> viaShow i
    MKStr   s      -> viaShow s
    MKBool  b      -> viaShow b
    MKTuple elts   -> pretty "(" <> tupled (map (genJl False) elts) <> pretty ",)"
    MKAnd a b      -> genJl False a <+> pretty "&&" <+> genJl False b
    MKOr  a b      -> genJl False a <+> pretty "||" <+> genJl False b

