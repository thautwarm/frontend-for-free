
import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import RBNF.Semantics
import RBNF.Graph
import RBNF.Dump

import Prelude hiding (writeFile)
import Data.Foldable
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO (writeFile)
import Data.Aeson
import Data.Text
import Control.Lens
import Control.Monad.State


import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

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
            CSeq [ CPred (MTerm "somePred"), CNonTerm "Factor" ],
            CSeq [ CNonTerm "Mul", "name" =:= multiply, CNonTerm "Factor"]
        ]
    ]

main = do
    putStrLn ""
    -- for_ (M.toList $ followSet $ mkGrammar parsers) $ \(a, b) ->
    --         putStr a >> putStrLn ":" >> putStrLn (L.intercalate ", " $ map show b)
    let ks = pGToSG  . markedLeftRecur $ mkGrammar parsers
    -- let prods' = M.toList $ view prods ks
    -- let leftR' = M.toList $ view leftR ks
    -- let syms = L.map fst prods'
    let ms = buildGraph ks
    writeFile "./test.json" (encodeToLazyText ms)
    -- putStrLn "left recursions:"
    -- for_ (M.toList $ _leftR ms) $ \(s, xs) -> do
    --     putStrLn $ "Rule:" ++ s
    --     for_ xs $ \s -> putStrLn "" >> print s
    -- putStrLn "not left recursions:"
    -- for_ (M.toList $ _prods ms) $ \(s, xs) -> do
    --     putStrLn $ "Rule:" ++ s
    --     for_ xs $ \s -> putStrLn "" >> print s

    -- for_ (productions ms) $ \s ->
    --     print s >> print "======="

    -- putStrLn "" >>
    -- (forM_ parsers $ putStrLn . show)
