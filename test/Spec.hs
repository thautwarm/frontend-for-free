
import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable

{-
Number ::= <number>
Add ::= Number | Add '+' Number
-}

infix 5 -->
infix 6 =:=
infix 6 |=

number = "number"
negation = "-"
multiply = "*"

a |= b = CBind a b

a --> b = (a, b, Nothing)
a =:= b = CTerm (Case a b)
parsers = S.fromList [
    "Number"   --> "name" =:= number
    , "Factor" --> CAlt [
            CNonTerm "Number",
            CSeq [ "name" =:= negation, "a" |= CNonTerm "Factor" ]
        ]
    , "Mul"    --> CAlt [
            CNonTerm "Factor",
            CSeq [ CNonTerm "Mul", "name" =:= multiply, CNonTerm "Factor"]
        ]
    ]

main = do
    putStrLn ""
    let ms = markedLeftRecur $ mkGrammar parsers
    for_ (leftRecurs ms) $ \s ->
        do print s >> print "======="

    for_ (productions ms) $ \s ->
        do print s >> print "======="


    -- putStrLn "" >>
    -- (forM_ parsers $ putStrLn . show)