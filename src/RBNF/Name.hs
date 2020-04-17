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

isJavaName :: forall t. Foldable t => t Char -> Bool
isJavaName = all isJavaChar where
  isJavaChar c | c `elem` ['a' .. 'z'] ||
                 c `elem` ['A' .. 'Z'] ||
                 c `elem` ['0' .. '9'] ||
                 c == '_' = True
               | otherwise = False

instance Show Name where
    show = \case
        Lexical s
            | "rbnf_tmp_" `isPrefixOf` s || "rbnf_named_" `isPrefixOf` s
            -> error $ printf "name %s break name mangling system" s
            | otherwise
            -> printf "%s" s
        Builtin s -> printf "builtin_%s" s
        Tmp i | i < 0     -> printf "rbnf_tmp_%d_" (-i)
              | otherwise -> printf "rbnf_tmp_%d" i
        NamedTmp s ->
            let rep '.' = '_'
                rep '-' = '_'
                rep a   = a
            in  map rep $ printf "rbnf_named_%s" s


