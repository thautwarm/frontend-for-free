{-# LANGUAGE MonadComprehensions #-}

import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import RBNF.Semantics
import RBNF.Graph
import RBNF.Dump
import RBNF.LookAHead
import RBNF.CodeGenIRs.A
import RBNF.CodeGen
import RBNF (parserGen)
import RBNF.Serialization
import RBNF.CodeGenIRs.B

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

import qualified Data.Text.Lazy.IO as T

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
case' a = CTerm a
parsers = CGrammar [
    "Number"   --> case' number
    -- , "Factor" --> CAlt [
    --         CNonTerm "Number",
    --         CSeq [case' negation, "a" |= CNonTerm "Factor" ]
    --     ]
    -- , "Mul"    --> CAlt [
    --         CSeq [ CPred (MTerm "somePred"), CNonTerm "Factor" ],
    --         CSeq [ CNonTerm "Mul", case' multiply, CNonTerm "Factor"]
    --     ]
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
    printAIR 80 a
test3 = do
    putStrLn ""
    let gbuilder = mkGrammar $ parsers
    let g = markedLeftRecur gbuilder
    let ks = pGToSG  g

    let graph = buildGraph ks
    let dectrees = M.map decideId3FromLATree $ makeLATables 1 graph
    let c = CompilationInfo {
            graph    = graph
            , decisions = dectrees
            , withTrace = True
            , isLeftRec = False
           }
    let s = "Mul"
        i = view starts graph M.! s
    putStrLn s
    let cfg = emptyCFG s


    printAIR 80 $ runToCode cfg $ codeGen c i

    putStrLn ""
    let s' = "Mul"
        i' = view ends graph M.! s'
    printAIR 80 $ runToCode cfg $ codeGen c {isLeftRec = True} i'

test4 = do
    putStrLn ""
    let a = flip evalState 0 $ aToB $ parserGen 1 False parsers
        f ::FixT (BIR Int) -> IO ()
        f a = forM_ (outT a) (\x -> print x >> putStrLn "" >> mapM_ f (outT x))
    f a

test5  = T.writeFile "a.txt" $ dumpCG parsers
main = test4