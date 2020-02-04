-- | This module provides name representations for various IRs.
module RBNF.Name where
import           RBNF.Utils
import           GHC.Generics
import           Data.List                      ( isPrefixOf )

data Name
    = Lexical String
    | Builtin String
    | NamedTmp String
    | Tmp Int
    deriving (Eq, Ord, Generic)


instance Show Name where
    show = \case
        Lexical s
            | "rbnf_tmp_" `isPrefixOf` s || "rbnf_namedtmp_" `isPrefixOf` s
            -> error $ printf "name %s break name mangling" s
            | otherwise
            -> printf "%s" s
        Builtin s -> printf "builtin_%s" s
        Tmp i | i < 0     -> printf "rbnf_tmp_%d_" (-i)
              | otherwise -> printf "rbnf_tmp_%d" i
        NamedTmp s ->
            let rep '.' = '_'
                rep '-' = '_'
                rep a   = a
            in  map rep $ printf "rbnf_namedtmp_%s" s


