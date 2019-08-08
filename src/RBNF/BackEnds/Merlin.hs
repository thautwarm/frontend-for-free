-- | Merlin stands for code generator targeting OCaml
{-# LANGUAGE OverloadedStrings #-}

module RBNF.BackEnds.Merlin where
import RBNF.IRs.Reimu
import RBNF.TypeSystem
import RBNF.Name
import RBNF.Utils
import GHC.Generics

import qualified Data.Text as T
-- | The spelling of Meirin is similar to Merlin.
--   Merlin is aka OCaml Programming Language's leading IDE plugin.
data OCamlBackEnd = OCamlBackEnd

mangleOC :: Text -> Text
mangleOC = mangling "." "_"

genericVar :: String -> Text
genericVar s = pack $ '\'':s

ocamlTypeText :: RT -> Text
ocamlTypeText = showText
    where
      showText :: RT -> Text
      showText = \case
        RTPrim  t    -> case t of
            RTInt    -> "int"
            RTFloat  -> "float"
            RTUnit   -> "()"
            RTString -> "str"
            RTAny    -> "any"
            RTBool   -> "bool"
            RTState  -> "state"

        RTVar s        -> pack s
        RTFunc a b     -> showNest a <> " -> " <> showText b
        RTTuple xs     -> "(" <> T.intercalate "*" (map showText xs) <> ")"
        RTGeneric xs t -> (T.unwords . map genericVar $ xs) <> ". " <> showText t
        RTApp t1 t2    -> showText t1 <> " " <> showNest t2
        RTSig s        -> mangleOC (pack $ show s)

      showNest s
            | isNest s  = "(" <> showText s <> ")"
            | otherwise = showText s
      isNest s = case s of
            RTFunc _ _    -> True
            RTGeneric _ _ -> True
            _             -> False

ocamlGen :: Reimu RT -> Text
ocamlGen reimu@InT {tag, outT} = case outT of
    _ -> error ""
