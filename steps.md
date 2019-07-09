# Steps

## BNF Grammar

```haskell


data Lexer where
    -- L = R"<String>"
    RegexL :: {name :: String, pattern :: Srring} -> Lexer
    -- L = "<String>"
    StrsL  :: {name :: String, values :: [String]} -> Lexer
    deriving (Eq, Ord, Show)

data Range where
    -- if both `atLeast` and `atMost` are given, the `repeat` parser
    --  could be translated into many `and` parsers.
    -- -- Range = '>=' <Int>
    -- LeR :: Int -> Range
    -- -- Range = '<=' <Int>
    -- GeR :: Int -> Range
    deriving (Eq, Ord, Show)

data Parser where
    -- P = P | P
    OrP      :: Parser -> Parser -> Parser
    -- P = P P
    AndP     :: Parser -> Parser -> Parser
    -- P = L
    LitP     :: Lexer  -> Parser
    -- P = P+
    RepP     :: Parser -> Parser
    -- P = <String>
    RefP     :: String -> Parser
    deriving (Eq, Ord, Show)
```

