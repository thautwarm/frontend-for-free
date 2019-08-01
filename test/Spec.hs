
import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import RBNF.Semantics
import RBNF.Graph
import RBNF.Dump
import RBNF.LookAHead

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
import Control.Arrow

{-
Number ::= <number>
Add ::= Number | Add '+' Number
-}

infix 5 -->
infix 6 |=

number = "number"
negation = "-"
multiply = "*"

a |= b = CBind a b

a --> b = (a, b, Nothing)
case' a = CTerm (Case a)
parsers = S.fromList [
    "Number"   --> case' number
    , "Factor" --> CAlt [
            CNonTerm "Number",
            CSeq [case' negation, "a" |= CNonTerm "Factor" ]
        ]
    , "Mul"    --> CAlt [
            CSeq [ CPred (MTerm "somePred"), CNonTerm "Factor" ],
            CSeq [ CNonTerm "Mul", case' multiply, CNonTerm "Factor"]
        ]
    ]

main = do
    putStrLn ""
    -- for_ (M.toList $ followSet $ mkGrammar parsers) $ \(a, b) ->
    --         putStr a >> putStrLn ":" >> putStrLn (L.intercalate ", " $ map show b)
    let gbuilder = mkGrammar $ parsers
    let g = markedLeftRecur gbuilder
    let ks = pGToSG  g
    let ps = view prods g
    -- let leftR' = M.toList $ view leftR ks
    -- let syms = L.map fst prods'
    let ms = buildGraph ks
    -- forM_ gbuilder print
    -- forM_ (view prods g) print
    writeFile "./test.json" (encodeToLazyText ms)
    let trees = M.map (id&&&decideId3FromLATree) $ makeLATables 1 ms
    forM_ (M.toList trees) $ \(i, (latree, id3tree)) -> do
        putStrLn $ "======== Node" ++ show i ++ " ========"
        putStrLn $ dispLATree 2 latree
        putStrLn "--- LA optimization:"
        putStrLn $ dispID3Tree 2 id3tree
        putStrLn ""
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