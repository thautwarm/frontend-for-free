-- | Merlin stands for code generator targeting OCaml
{-# LANGUAGE OverloadedStrings #-}

module RBNF.BackEnds.Merlin where
import           RBNF.IRs.Reimu
import           RBNF.IRs.MarisaLibrary         ( dsl_s_to_i_n )
import           RBNF.TypeSystem
import           RBNF.Name
import           RBNF.Utils
import           GHC.Generics
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.State
import           Data.Text.Prettyprint.Doc
import           Codec.Binary.UTF8.String      as UTF8

import           Control.Monad.Identity

import qualified Data.Text                     as T
import qualified Data.Set                      as S
import qualified Data.Map                      as M

-- | The spelling of Meirin is similar to Merlin.
--   Merlin is aka OCaml Programming Language's leading IDE plugin.

showVar :: MName -> Text
showVar = mangleOC . pack . show

mangleOC :: Text -> Text
mangleOC = mangling "." "_" . mangling "%" "prim_" . mangling "-" "local_"

genericVar :: String -> Text
genericVar s = pack $ '\'' : s

ocamlTypeText :: RT -> Doc ann
ocamlTypeText = pretty . showText
  where
    showText :: RT -> Text
    showText = \case
        RTPrim t -> case t of
            RTInt    -> "int"
            RTFloat  -> "float"
            RTUnit   -> "()"
            RTString -> "str"
            RTAny    -> "any"
            RTBool   -> "bool"
            RTState  -> "state"

        RTVar s    -> pack s
        RTFunc a b -> showNest a <> " -> " <> showText b
        RTTuple xs -> "(" <> T.intercalate " * " (map showText xs) <> ")"
        RTGeneric xs t ->
            (T.unwords . map genericVar $ xs) <> ". " <> showText t
        RTApp t1 t2 -> showText t1 <> " " <> showNest t2
        RTSig s     -> showVar s
    showNest s | isNest s  = "(" <> showText s <> ")"
               | otherwise = showText s
    isNest s = case s of
        RTFunc    _ _ -> True
        RTGeneric _ _ -> True
        _             -> False

isRelaxedValTy = \case
    RTGeneric _ (RTVar _) -> True
    _                     -> False

-- I don't want to hold a table in monads to check if a variable is mutable.
-- thus, I translate all variables into 'a ref...

tupleLength = \case
    RTTuple xs    -> length xs
    RTGeneric _ x -> tupleLength x
    _             -> error "impossible due to type checking"

encodeTokenName s =
    ("(* " ++ s ++ " *)")
        <> "token_id_of_"
        <> (intercalate "_" . map show $ UTF8.encode s)
caseInt InT { tag, outT = RCall _ [InT { outT = RStr s }] } =
    pretty $ encodeTokenName s

stmtContinue InT { tag, outT = RDecl _ _ } = emptyDoc
stmtContinue _                             = pretty (";" :: Text)


resolveTokensM :: Reimu a -> State (Set (String, String)) ()
resolveTokensM a@InT { outT } = case outT of
    RCall InT { outT = RVar n } [InT { outT = RStr s }] | dsl_s_to_i_n == n ->
        modify (S.insert (s, encodeTokenName s))
    _ -> traverse_ resolveTokensM outT

resolveTokens :: Reimu a -> Doc ann
resolveTokens a =
    vsep
        $ [ "let"
            <+> pretty n'
            <+> ": int"
            <+> "="
            <+> "Hashtbl.find LexerInfo.prim_token_names"
            <+> pretty (show n)
          | (n, n') <- S.toList $ execState (resolveTokensM a) S.empty
          ]

data MutableScolver = MutableSolver {
          _cnt        :: Int
        , _nameToSlot :: Map MName Int
        , _slotToMut  :: Map Int Bool
    }

emptyMutableScolver = MutableSolver 0 M.empty M.empty

makeLenses ''MutableScolver

withScope :: State MutableScolver a -> State MutableScolver a
withScope m = do
    s <- gets $ view nameToSlot
    r <- m
    modify $ over nameToSlot (const s)
    return r

resolveMutable :: Reimu RT -> State MutableScolver (Reimu (Int, RT))
resolveMutable InT { tag = t, outT = base } = case base of
    -- to simply, we mark parameters as immutable bound variables.
    RDecl n v -> do
        v       <- resolveMutable v
        scopeId <- modify (over cnt (+ 1)) >> gets (view cnt)
        modify $ over nameToSlot (M.insert n scopeId)
        modify $ over slotToMut (M.insert scopeId False)
        return InT { tag = (scopeId, t), outT = RDecl n v }
    RAssign n v -> do
        -- only mutable locals can be assigned.
        v       <- resolveMutable v
        scopeId <- gets $ (M.! n) . view nameToSlot
        -- See! it's now mutable!
        modify $ over slotToMut (M.insert scopeId True)
        return InT { tag = (scopeId, t), outT = RAssign n v }
    RVar n -> do
        maybeScopeId <- gets $ (n `M.lookup`) . view nameToSlot
        let scopeId = case maybeScopeId of
                Just scopeId -> scopeId
                _            -> 0 -- parameters or globals
        return InT { tag = (scopeId, t), outT = RVar n }
-- if, while, switch has to resume the context after getting out of a branch.
    RIf cond a b -> do
        cond <- resolveMutable cond
        a    <- withScope $ resolveMutable a
        b    <- withScope $ resolveMutable b
        return InT { tag = (0, t), outT = RIf cond a b }
    RSwitch test cases defau -> do
        test  <- resolveMutable test
        cases <- forM cases $ \(a, b) -> withScope $ do
            a <- resolveMutable a
            b <- resolveMutable b
            return (a, b)
        defau <- withScope $ resolveMutable defau
        return InT { tag = (0, t), outT = RSwitch test cases defau }
    RWhile cond body -> do
        cond <- resolveMutable cond
        body <- withScope $ resolveMutable body
        return InT { tag = (0, t), outT = RWhile cond body }
    RDef _ _ -> withScope $ do
        modify $ over nameToSlot (const M.empty)
        default'
    _ -> default'
  where
    default' = do
        base <- traverse resolveMutable base
        return InT { tag = (0, t), outT = base }

ocamlGenNested :: Map Int Bool -> Reimu (Int, RT) -> Doc ann
ocamlGenNested mutableInfo v@InT { outT = base }
    | isNested base = parens . runIdentity . ocamlGenM mutableInfo $ v
    | otherwise     = runIdentity . ocamlGenM mutableInfo $ v
  where
    isNested = \case
        RAssign _ _   -> True
        RDecl   _ _   -> True
        RIf _ _ _     -> True
        RWhile _ _    -> True
        RSwitch _ _ _ -> True
        RDef    _ _   -> True
        RMutual _ _   -> True
        RCall   _ _   -> True
        -- RAttr _ _  -> False
        -- RPrj  _ _  -> False
        -- RVar _      -> False
        RAnd    _ _   -> True
        ROr     _ _   -> True
        RExtern _ _ x -> isNested $ outT x
        _             -> False

tupleDoc :: [Doc ann] -> Doc ann
tupleDoc xs = parens . sep $ punctuate comma xs

liftId :: (a -> Identity b) -> Identity (a -> b)
liftId f = return $ \a -> runIdentity $ f a

ocamlGenM :: Map Int Bool -> Reimu (Int, RT) -> Identity (Doc ann)
ocamlGenM mutableInfo InT { tag = (scopeId, t), outT } = do
    let myty | isRelaxedValTy t = RTVar "relax"
             | otherwise        = t
    ocamlGenM      <- liftId $ ocamlGenM mutableInfo
    ocamlGenNested <- return $ ocamlGenNested mutableInfo
    return $ case outT of
        RAssign n a -> pretty (showVar n) <+> " := " <+> ocamlGenM a
        RDecl n a
            | mutableInfo M.! scopeId -> vsep
                [ nest 4 $ vsep
                    [ "let"
                    <+> pretty (showVar n)
                    <+> ":"
                    <+> "("
                    <>  ocamlTypeText myty
                    <>  ") ref"
                    <+> "="
                    , "ref" <+> ocamlGenNested a
                    ]
                , "in"
                ]
            | otherwise -> vsep
                [ nest 4 $ vsep
                    [ "let"
                    <+> pretty (showVar n)
                    <+> ":"
                    <+> ocamlTypeText myty
                    <+> "="
                    , ocamlGenM a
                    ]
                , "in"
                ]

        RCall InT { outT = RVar n } [InT { outT = RStr s }]
            | dsl_s_to_i_n == n -> pretty $ encodeTokenName s
        RCall f args -> ocamlGenM f <+> parens (sep $ map ocamlGenNested args)
        RAttr a attr -> ocamlGenM a <+> "." <> pretty attr
        -- e.g. a : ('a, 'b, 'c), 'a[2]' translated to '(match a with (_, _, a) -> a)'
        RPrj a i ->
            "(match"
                <+> ocamlGenM a
                <+> "with"
                <+> tupleDoc
                        [ if i == i' then "a" else "_"
                        | i' <- [0 .. tupleLength myty - 1]
                        ]
                <>  " -> a)"
        RIf cond tCls fCls -> align $ vsep
            [ "if" <+> ocamlGenM cond
            , "then" <+> ocamlGenM tCls
            , "else" <+> ocamlGenM fCls
            ]
        RWhile cond body ->
            vsep
                [ nest 2 $ vsep ["while" <+> ocamlGenM cond, ocamlGenM body]
                , "done"
                ]
        RSwitch test cases defau -> align $ vsep
            [ "begin"
            , "match" <+> ocamlGenM test <+> "with"
            , vsep
                [ "|" <+> ocamlGenM case' <+> "->" <+> (nest 2 $ ocamlGenM body)
                | (case', body) <- cases
                ]
            , "| _ -> " <+> ocamlGenM defau
            , "end"
            ]
        RDef args body ->
            nest 2
                $ vsep
                      [ "fun"
                      <+> tupleDoc (map (pretty . showVar) args)
                      <+> "->"
                      , ocamlGenM body
                      ]
        RMutual [] exprs -> nest 2 $ vsep
            [ "begin"
            , vsep
                [
                    -- if not `let a = b`, get followed by a ";"
                  ocamlGenM expr <+> stmtContinue expr
                | expr <- exprs
                ]
            , "end"
            ]
        RMutual names exprs ->
            let n1 = length names
                n2 = length exprs
                head :: Int -> Doc ann
                head 0 = "let rec"
                head i = "and"
                gen i n (InT {outT = RDef args body}) = vsep
                    [ head i
                    <+> (pretty . showVar) n
                    <+> (tupleDoc $ map (pretty . showVar) args)
                    <+> "="
                    , ocamlGenM body
                    ]
            in  case n1 `compare` n2 of
                    EQ -> -- for OCaml backend, only support toplevel mutual block
                        vsep $ zipWith3 gen [0 ..] names exprs
                    _ -> error "not supported yet"
        RVar name
            | scopeId /= 0 && mutableInfo M.! scopeId -> "!"
            <> (pretty . showVar) name
            | otherwise -> pretty $ showVar name
        RInt   i      -> viaShow i
        RStr   s      -> pretty s
        RBool  True   -> "true"
        RBool  False  -> "false"
        -- there's no 1-length tuple, so just tupleDoc
        RTuple xs     -> tupleDoc $ map ocamlGenM xs
        RAnd a b      -> ocamlGenM a <+> "&&" <+> ocamlGenM b
        ROr  a b      -> ocamlGenM a <+> "||" <+> ocamlGenM b
        RExtern _ _ a -> ocamlGenM a

ocamlGen :: Reimu RT -> Doc ann
ocamlGen a =
    let
        tokenInfos = resolveTokens a
        (a', m)    = runState (resolveMutable a) emptyMutableScolver
        parsers    = runIdentity $ ocamlGenM (view slotToMut m) a'
    in
        vsep
            [ pretty ("module GeneratedParser (" :: String)
            , pretty
                ("LexerInfo: sig val prim_token_names : (string, int) Hashtbl.t end" :: String
                )
            , pretty (") = struct" :: String)
            , tokenInfos
            , parsers
            , pretty ("end" :: String)
            ]
