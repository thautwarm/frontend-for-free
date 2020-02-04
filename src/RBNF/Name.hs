-- | This module provides name representations for various IRs.
module RBNF.Name where
import           RBNF.Utils
import           GHC.Generics


data Name
    = Lexical String
    | Builtin String
    | NamedTmp String
    | Tmp Int
    deriving (Eq, Ord, Generic)


instance Show Name where
    show = \case
        Lexical s -> printf "lexical_%s" s
        Builtin s -> printf "builtin_%s" s
        Tmp i | i < 0     -> printf "tmp_%d_" (-i)
              | otherwise -> printf "tmp_%d" i
        NamedTmp s ->
            let rep '.' = '_'
                rep '-' = '_'
                rep a   = a
            in  map rep $ printf "namedtmp_%s" s


