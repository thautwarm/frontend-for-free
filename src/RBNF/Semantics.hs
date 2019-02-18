{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}

module RBNF.Semantics where
import qualified Data.Map as M

-- class Tokenizer t where
--     lineno   :: t -> Int
--     colno    :: t -> Int
--     filename :: t -> String
--     str      :: t -> String
--     kind     :: t -> String

data Lexer where
    RefL   :: String -> Lexer
    RegexL :: String -> Lexer
    StrsL  :: [String] -> Lexer
    deriving (Eq, Ord, Show)

data Range where
    -- if `atLeast` and `atMost` are both given, the `repeat` parser
    --  could be translated into many `and` parsers.
    LessThan :: Int -> Range
    MoreThan :: Int -> Range
    deriving (Eq, Ord, Show)

data Parser where
    OrP      :: Parser -> Parser -> Parser
    AndP     :: Parser -> Parser -> Parser
    LitP     :: Lexer -> Parser
    RepP     :: Parser -> Range -> Parser
    RefP     :: String -> Parser
    deriving (Eq, Ord, Show)

type ParserContext = M.Map String Parser
type LexerContext = M.Map String Lexer
