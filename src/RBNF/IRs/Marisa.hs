-- | Marisa is an IR sufficient for dynamic languages to codegen
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-03
-- License: BSD-3-clause
module RBNF.IRs.Marisa where
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                   ( Generic )
import           RBNF.Name
import           RBNF.HMTypeInfer               ( HMT )
import           RBNF.TypeSystem                ( RT )

-- | MK for Marisa Kirisame
-- There's a punning case as the abbreviation of "make"
-- is also MK and its meaning makes sense here.
data Marisa
    = MKAssign MName Marisa
    | MKCall Marisa [Marisa]
    | MKAttr Marisa String
    -- statiically projections on tuples
    | MKPrj    Marisa Int
    | MKIf     Marisa Marisa Marisa
    | MKWhile  Marisa Marisa
    | MKSwitch Marisa [(Marisa, Marisa)] Marisa
    | MKDef    MName [MName] Marisa
    | MKBlock  [Marisa]
    | MKVar    MName
    | MKInt    Int
    | MKStr    String
    | MKBool   Bool
    | MKTuple  [Marisa]
    | MKAnd    Marisa Marisa
    | MKOr     Marisa Marisa
    -- Either Type [(FieldName, Type)]
    -- if is Left, it's global variable's type declaration
    -- if is Right, it's structure type's declaration.
    | MKExtern MName (Either (HMT RT) ([String], [(String, HMT RT)])) Marisa

deriving instance Eq Marisa
deriving instance Ord Marisa
deriving instance Generic Marisa


isSimpleMarisa = \case
    MKInt  _   -> True
    MKStr  _   -> True
    MKBool _   -> True
    MKVar  _   -> True
    MKAttr _ _ -> True
    MKCall _ _ -> True
    MKPrj  _ _ -> True
    MKTuple xs -> all isSimpleMarisa xs
    MKIf x y z -> isSimpleMarisa x && isSimpleMarisa y && isSimpleMarisa z
    _          -> False

seeMarisa = align . \case
    MKAssign n (MKBlock codes) ->
        nest 4 $ sep $ pretty (show n ++ " =") : map seeMarisa codes
    MKAssign n code@(isSimpleMarisa -> False) ->
        nest 4 $ vsep [pretty (show n ++ " = "), seeMarisa code]
    MKAssign n code -> pretty (show n ++ " = ") <+> seeMarisa code
    MKCall f args ->
        seeMarisa f <> (parens . sep . punctuate comma $ map seeMarisa args)
    MKAttr val attr   -> seeMarisa val <> pretty ("." ++ attr)
    MKPrj  val dim    -> seeMarisa val <> brackets (viaShow dim)
    MKIf cond br1 br2 -> vsep
        [ pretty "if" <+> nest 4 (seeMarisa cond)
        , pretty "then" <+> nest 4 (seeMarisa br1)
        , pretty "else" <+> nest 4 (seeMarisa br2)
        ]
    MKWhile cond br ->
        nest 4 $ vsep [pretty "while" <+> seeMarisa cond, seeMarisa br]
    MKSwitch expr cases default' -> vsep
        [ pretty "switch" <+> seeMarisa expr
        , nest 2 $ align $ vsep
            [ nest 2
                  $ vsep
                        [ pretty "case" <+> (seeMarisa i) <+> pretty ":"
                        , seeMarisa case'
                        ]
            | (i, case') <- cases
            ]
        , pretty "default :" <+> nest 4 (seeMarisa default')
        ]
    MKDef fname args body ->
        let
            fnName = viaShow fname
            argDef = sep $ punctuate comma $ map viaShow args
        in
            nest 4
                $ vsep
                      [ pretty "def" <+> fnName <> parens argDef
                      , align $ nest 4 $ seeMarisa body
                      ]
    MKBlock suite  -> vsep $ map seeMarisa suite
    MKVar   n      -> viaShow n
    MKInt   i      -> viaShow i
    MKStr   s      -> viaShow s
    MKBool  b      -> viaShow b
    MKTuple elts   -> pretty "tuple" <> tupled (map seeMarisa elts)
    MKAnd a b      -> seeMarisa a <+> pretty "and" <+> seeMarisa b
    MKOr  a b      -> seeMarisa a <+> pretty "or" <+> seeMarisa b
    MKExtern n (Right t) m -> vsep
        [ sep [pretty "external", viaShow n, pretty (":" ++ show t)]
        , seeMarisa m
        ]
    MKExtern n (Left t) m -> vsep
        [ sep [pretty "external", viaShow n, pretty (":" ++ show t)]
        , seeMarisa m
        ]

instance Pretty Marisa where
    pretty = seeMarisa