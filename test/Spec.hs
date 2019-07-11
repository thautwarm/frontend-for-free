
import RBNF.Grammar
import RBNF.Default
import qualified Data.Map as M
import Data.Foldable

{-
Number ::= <number>
Add ::= Number | Add '+' Number
-}

infix 5 -->
infix 6 =:=

number = "number"
negation = "-"
multiply = "*"

a --> b = (a, b)
a =:= b = CTerm [MatchCond a b]
parsers = CGrammar . M.fromList $ [
    "Number" --> "name" =:= number
    , "Factor" --> CAlt[ CNonTerm "Number", CSeq [ "name" =:= negation, CNonTerm "Factor" ]]
    , "Mul"    --> CAlt[ CNonTerm "Factor", CSeq [ CNonTerm "Mul", "name" =:= multiply, CNonTerm "Factor"]]
    ]

mkMyGrammar :: MkGrammar SimplePGrammar
mkMyGrammar = mkGrammar

main = do
    putStrLn ""
    print $ mkMyGrammar parsers

    -- putStrLn "" >>
    -- (forM_ parsers $ putStrLn . show)