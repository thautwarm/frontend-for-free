
import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import RBNF.Semantics
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
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
            CSeq ["name" =:= negation, "a" |= CNonTerm "Factor" ]
        ]
    , "Mul"    --> CAlt [
            CNonTerm "Factor",
            CSeq [ CNonTerm "Mul", "name" =:= multiply, CNonTerm "Factor"]
        ]
    ]

main = do
    putStrLn ""
    -- for_ (M.toList $ followSet $ mkGrammar parsers) $ \(a, b) ->
    --         putStr a >> putStrLn ":" >> putStrLn (L.intercalate ", " $ map show b)

    let ms = pGToSG  . markedLeftRecur $ mkGrammar parsers
    putStrLn "left recursions:"
    for_ (M.toList $ _leftR ms) $ \(s, xs) -> do
        putStrLn $ "Rule:" ++ s
        for_ xs $ \s -> putStrLn "" >> print s
    putStrLn "not left recursions:"
    for_ (M.toList $ _prods ms) $ \(s, xs) -> do
        putStrLn $ "Rule:" ++ s
        for_ xs $ \s -> putStrLn "" >> print s

    -- for_ (productions ms) $ \s ->
    --     print s >> print "======="

    -- putStrLn "" >>
    -- (forM_ parsers $ putStrLn . show)
