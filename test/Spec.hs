
import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import RBNF.Semantics hiding (CFG, emptyCFG)
import RBNF.Graph
import RBNF.Dump
import RBNF.LookAHead
import RBNF.BackEnd
import RBNF.CodeGen

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

test1 = do
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
    print ms
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
test2 = do
    putStrLn ""
    let a = AIf (dsl_int 1) (dsl_int 2) (dsl_int 3)
    codeToString 80 a
test3 = do
    putStrLn ""
    let gbuilder = mkGrammar $ parsers
    let tokenNames = S.toList $ collectTokenNames gbuilder
    let g = markedLeftRecur gbuilder
    let ks = pGToSG  g
    -- forM_ (view prods g) print
    -- forM_ (view prods ks) print
    let graph = buildGraph ks
    let dectrees = M.map decideId3FromLATree $ makeLATables 1 graph
    let c = CompilationInfo {
              tokenIds = M.fromList $ L.zip tokenNames [0, 1..]
            , graph    = graph
            , decisions = dectrees
            , withTrace = False
           }
    let (s, i) = L.head $ M.toList $ view starts graph
    putStrLn s
    let cfg = emptyCFG s (AName "tokens") (AName "offname") (AName "state")
    let code = runToCode cfg $ codeGen c i
    codeToString 80 $ code
main = test3