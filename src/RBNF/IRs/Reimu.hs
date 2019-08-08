-- | Based on IR Marisa, after inserting declarations,
--   making it extensible(like capable of introducing type inference)
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-07-13
-- License: BSD-3-clause
module RBNF.IRs.Reimu
(Reimu, ReimuBase(..), seeReimu, TaggedFixT(..))
where
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                   ( Generic )
import           RBNF.Name
import           RBNF.HMTypeInfer               ( HMT )
import           RBNF.TypeSystem                ( RT )

data ReimuBase a
    = RAssign MName a
    | RDecl MName a
    | RCall a [a]
    | RAttr a String
    | RPrj  a Int
    | RIf a a a
    | RWhile a a
    | RSwitch a [(a, a)] a
    | RDef [MName] a
    | RMutual [MName] [a]
    -- literal
    |  RVar MName
    | RInt Int
    | RStr String
    | RBool Bool
    | RTuple [a]
    | RAnd a a
    | ROr  a a
    | RExtern MName (Either (HMT RT) ([String], [(String, HMT RT)])) a


deriving instance Eq a => Eq (ReimuBase a)
deriving instance Ord a => Ord (ReimuBase a)
deriving instance Generic (ReimuBase a)
deriving instance Functor ReimuBase
deriving instance Foldable ReimuBase
deriving instance Traversable ReimuBase

data TaggedFixT f t = InT {tag :: t, outT :: f (TaggedFixT f t)}

deriving instance Eq b => Eq (TaggedFixT ReimuBase b)
deriving instance Ord b => Ord (TaggedFixT ReimuBase b)
deriving instance Functor f => Functor (TaggedFixT f)
deriving instance Foldable f => Foldable (TaggedFixT f)
deriving instance Traversable f => Traversable (TaggedFixT f)

type Reimu a = TaggedFixT ReimuBase a

isSimpleReimu = \case
    RInt  _       -> True
    RStr  _       -> True
    RBool _       -> True
    RVar  _       -> True
    RAttr   _  _  -> True
    RCall   _  _  -> True
    RPrj    _  _  -> True
    RMutual ns xs -> length ns > 1 || all isSimpleReimu (map outT xs)
    RTuple xs     -> all isSimpleReimu (map outT xs)
    RIf x y z     -> all isSimpleReimu $ map outT [x, y, z]
    _             -> False


seeReimu :: (Show a) => Reimu a -> Doc ann
seeReimu = frec
  where
    frec InT { tag = t, outT = base } = let ts = show t in
        align $ case base of

            RDecl n v@InT { outT = isSimpleReimu -> False } ->
                nest 4
                    $ vsep
                            [ pretty ("var " ++ show n ++ " :" ++ ts ++ " =")
                            , frec v
                            ]
            RDecl n code ->
                pretty ("var " ++ show n ++ " :" ++ ts ++ " = ")
                    <+> frec code

            RAssign n v@InT { outT = isSimpleReimu -> False } -> nest 4
                $ vsep [pretty (show n ++ " :" ++ ts ++ " ="), frec v]
            RAssign n code ->
                pretty (show n ++ " :" ++ ts ++ " = ") <+> frec code

            RCall f args -> vsep
                [ pretty ("[" ++ ts ++ "]")
                , frec f <> (parens . sep . punctuate comma $ map frec args)
                ]
            RAttr val attr ->
                pretty ("[" ++ ts ++ "]") <> frec val <> pretty
                    ("." ++ attr)
            RPrj val dim ->
                pretty ("[" ++ ts ++ "]") <> frec val <> brackets
                    (viaShow dim)
            RIf cond br1 br2 -> vsep
                [ pretty ("[" ++ ts ++ "]") <> pretty "if" <+> nest
                    4
                    (frec cond)
                , pretty "then" <+> nest 4 (frec br1)
                , pretty "else" <+> nest 4 (frec br2)
                ]
            RWhile cond br ->
                nest 4
                    $ vsep
                            [ pretty ("[" ++ ts ++ "]")
                            <>  pretty "while"
                            <+> frec cond
                            , frec br
                            ]
            RSwitch expr cases default' -> vsep
                [ pretty "switch" <+> frec expr
                , nest 2 $ align $ vsep
                    [ nest 2
                            $ vsep
                                [ pretty "case" <+> (frec i) <+> pretty ":"
                                , frec case'
                                ]
                    | (i, case') <- cases
                    ]
                , pretty "default :" <+> nest 4 (frec default')
                ]
            RDef args body ->
                let argDef = sep $ punctuate comma $ map viaShow args
                in
                    nest 4
                        $ vsep
                                [ parens argDef <+> pretty "->"
                                , align $ nest 4 $ frec body
                                ]
            RVar  n -> viaShow n
            RInt  i -> viaShow i
            RStr  s -> viaShow s
            RBool b -> viaShow b
            RTuple elts ->
                pretty ("[" ++ ts ++ "]") <> pretty "tuple" <> tupled
                    (map frec elts)

            RMutual names suite ->
                let
                    n         = length names
                    (l1, l2)  = splitAt n suite
                    recursive = zipWith
                        (\name v ->
                            nest 4
                                $ vsep
                                        [ pretty
                                            (  "rec "
                                            ++ show name
                                            ++ " : "
                                            ++ ts
                                            ++ " ="
                                            )
                                        , frec v
                                        ]
                        )
                        names
                        l1
                in
                    vsep $ recursive ++ map frec l2
            RAnd a b -> frec a <+> pretty "and" <+> frec b
            ROr  a b -> frec a <+> pretty "or" <+> frec b
            RExtern n (Left t) m -> vsep
                [ sep [pretty "external", viaShow n, pretty (":" ++ show t)]
                , frec m
                ]
            RExtern n (Right t) m -> vsep
                [ sep [pretty "external", viaShow n, pretty (":" ++ show t)]
                , frec m
                ]

instance Show a => Pretty (Reimu a) where
    pretty = seeReimu