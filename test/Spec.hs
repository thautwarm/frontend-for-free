import RBNF.Semantics
import RBNF.GraphAnalysis.Expand
import qualified Data.Map as M
import Data.Foldable

{-
Number ::= <number>
Add ::= Number | Add '+' Number 
-}
infixl 9  <&>
infixl  5  <|>
(<&>) = AndP
(<|>) = OrP
infixl 1 `produce`
a `produce` b = (a, b)

parsers =
    M.fromList [
        "Number" `produce` (LitP . RegexL) "<number regex>",
        "Add"    `produce` RefP "Number"
                    <|> RefP "Add" <&> (LitP . StrsL) ["+"] <&> RefP "Number"
    ]

main :: IO ()
main = do
    putStrLn "\n"
    for_ (M.toList $ expand parsers) f
    where
        f (k, xs) = do
            putStrLn $ show k ++ " -> \n"
            for_ xs $ \each ->
                putStrLn $ "     " ++ show each ++ "\n"


