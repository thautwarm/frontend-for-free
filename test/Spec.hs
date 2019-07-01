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
infixl 1 -->
a --> b = (a, b)

regex = LitP . RegexL

str x = LitP $ StrsL [x]

parsers =
    M.fromList [
        "Number" --> regex "<number regex>"
        , "Factor" --> RefP "Number" <|>  str "-" <&> RefP "Factor"
        , "Mul"    --> RefP "Factor" <|> RefP "Mul" <&> str "*" <&> RefP "Factor"
        -- , "Add"    --> RefP "Mul" <|> (RefP "Add" <&> str "+" <&> RefP "Mul")
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


