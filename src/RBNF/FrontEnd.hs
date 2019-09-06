{-# LANGUAGE FlexibleContexts #-}

module RBNF.FrontEnd where
import           RBNF.Utils
import           RBNF.Symbols
import           Control.Applicative
import           Prelude                 hiding ( fail )
import           Control.Monad           hiding ( fail )
import           Control.Monad.Fail             ( MonadFail(..) )
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Char                     as DC
import qualified Data.List                     as L

newtype Parser a = Parser {runParser :: (String, Loc) -> Either (String, Loc) (a, String, Loc)}
    deriving (Functor)

data Loc = Loc {col :: Int, line :: Int}
    deriving (Eq, Show, Ord)

instance Applicative Parser where
    pure a = Parser $ \(s, loc) -> return (a, s, loc)
    (<*>) = ap

instance MonadFail Parser where
    fail s = Parser $ \(_, loc@Loc { line, col }) ->
        let msg = s ++ ", at line " ++ show line ++ ", column " ++ show col
        in  Left (msg, loc)

instance Monad Parser where
    m >>= k = Parser $ \arg -> runParser m arg
        >>= \(a, s, loc) -> let f' = k a in runParser f' (s, loc)

instance Alternative Parser where
    empty = fail "Failed"
    p1 <|> p2 = Parser $ \(s, loc) -> case runParser p1 (s, loc) of
        err@(Left (_, loc')) ->
            if loc == loc' then runParser p2 (s, loc) else err
        a -> a

instance Semigroup a => Semigroup (Parser a) where
    p1 <> p2 = p1 >>= \a -> p2 >>= \b -> return $ a <> b

eps = Parser $ \(s, loc) -> return ((), s, loc)
instance Monoid a => Monoid  (Parser a) where
    mempty = fmap (const mempty) eps

char :: Char -> Parser Char
char c = token (show c, (== c))

noneOf :: [Char] -> Parser Char
noneOf cs = token ("none of " ++ show cs, (`notElem` cs))

oneOf :: [Char] -> Parser Char
oneOf cs = token ("one of " ++ show cs, (`elem` cs))

token :: (String, Char -> Bool) -> Parser Char
token (info, f) = Parser $ \case
    ([], loc@Loc { col, line }) ->
        let msg =
                    "got EOF, expect "
                        ++ show info
                        ++ " at line "
                        ++ show line
                        ++ ", column "
                        ++ show col
        in  Left (msg, loc)
    (c : tl, Loc { col, line }) | f c ->
        let loc' | c == '\n' = Loc { col = 0, line = line + 1 }
                 | otherwise = Loc { col = col + 1, line = line }
        in  return (c, tl, loc')
    (c' : _, loc@Loc { col, line }) ->
        let msg =
                    "expect "
                        ++ show info
                        ++ ", got "
                        ++ show c'
                        ++ ", at line "
                        ++ show line
                        ++ ", column "
                        ++ show col
        in  Left (msg, loc)

string :: String -> Parser String
string [] = mempty
string xs = do
    let collect []       = return []
        collect (x : xs) = do
            a <- char x
            (a :) <$> collect xs
    collect xs

escape :: Parser Char
escape = do
    char '\\'
    oneOf "\\\""

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

character :: Parser Char
character = nonEscape <|> escape

quotedStr :: Parser String
quotedStr = do
    char '"'
    string <- many character
    char '"'
    return $ "quote " ++ string

infixl 4 <***>
(<***>) p1 p2 = do
    a <- p1
    b <- p2
    return (a, b)

identifier = do
    hd <- token ("is identifier start", DC.isLetter)
    tl <- many $ token ("is identifier", trailer)
    return $ hd : tl
    where trailer x = any ($ x) [DC.isLetter, DC.isNumber, (== '_')]

unlist _    [hd] = hd
unlist cons xs   = cons xs

whiteSpace = do
    many $ oneOf " \n\r\t"
    return ()

la :: (String -> Parser a) -> Parser a
la laF = Parser $ \(s, loc) -> runParser (laF s) (s, loc)

strip :: String -> Parser a -> String -> Parser a
strip s p = \case
    ' ' : xs -> strip s p xs
    xs       -> p'
      where
        p' | L.isPrefixOf s xs = p
           | otherwise         = fail ("Expect starting with " ++ show s)

bound p = do
    r <- p
    whiteSpace
    return r

laChar s = la (strip [s] (whiteSpace *> char s))
laString s = la (strip s (whiteSpace *> string s))

termP = bound $ CTerm <$> (identifier <|> quotedStr)
nonTermP = bound $ CNonTerm <$> (char '<' *> identifier <* char '>')
predP = bound $ CPred <$> (char '?' *> miniP)
nestedP = bound $ char '(' *> bound cP <* char ')'
bindP =
    bound $ uncurry CBind <$> (char '!' *> identifier <* char '=' <***> atomP)

atomP = bound (nonTermP <|> bindP <|> predP <|> nestedP <|> termP)

cPSeq :: Parser [C]
cPSeq = sepBy whiteSpace atomP

cPOr :: Parser [C]
cPOr = unlist CSeq <$> cPSeq >>= \hd ->
    let cons = char '|' *> whiteSpace *> cPOr
        nil  = fmap (const []) eps
    in  (hd :) <$> (cons <|> nil)

cP :: Parser C
cP = CAlt <$> (whiteSpace *> cPOr)

optionP :: Parser a -> Parser (Maybe a)
optionP p = fmap Just p <|> fmap (const Nothing) eps

-- production def
stmtP :: Parser CProd
stmtP = do
    n <- bound identifier
    bound $ string "::="
    combinatoric <- cP
    reduce <- optionP (bound (string "->")  *> miniP)
    bound $ char ';'
    return (n, combinatoric, reduce)

modP :: Parser CGrammar
modP = CGrammar <$> (whiteSpace *> bound (many stmtP))

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = do
    h <- p
    (h :) <$> ((sep *> sepBy sep p) <|> fmap (const []) eps)

miniP :: Parser MiniLang
miniP = bound $ do
    f <- bound identifier
    let app = do
            bound $ char '('
            args <- sepBy (bound $ laChar ',') miniP
            bound $ char ')'
            return $ MApp f args
    app <|> fmap (const $ MTerm f) eps

parseDoc :: String -> Either String (CGrammar, String)
parseDoc s = case runParser modP (s, Loc 0 0) of
    Left  (a, _)    -> Left a
    Right (a, s, _) -> Right (a, s)

