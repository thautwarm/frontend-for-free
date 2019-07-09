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
    -- L = <String>
    RefL   :: String -> Lexer
    -- L = R"<String>"
    RegexL :: String -> Lexer
    -- L = "<String>"
    StrsL  :: [String] -> Lexer
    deriving (Eq, Ord, Show)

data Range where
    -- if both `atLeast` and `atMost` are given, the `repeat` parser
    --  could be translated into many `and` parsers.

    -- Range = '>=' <Int>
    LER :: Int -> Range
    -- Range = '<=' <Int>
    GER :: Int -> Range
    deriving (Eq, Ord, Show)

data Parser where
    -- P = P | P
    OrP      :: Parser -> Parser -> Parser
    -- P = P P
    AndP     :: Parser -> Parser -> Parser
    -- P = L
    LitP     :: Lexer  -> Parser
    -- P = P '{' Range '}'
    RepP     :: Parser -> Parser
    -- P = <String>
    RefP     :: String -> Parser
    deriving (Eq, Ord, Show)

type ParserContext = M.Map String Parser
type LexerContext = M.Map String Lexer
