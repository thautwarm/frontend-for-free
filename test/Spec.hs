{-# LANGUAGE MonadComprehensions #-}

import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import RBNF.Semantics
import RBNF.Graph
import RBNF.LookAHead
import RBNF.Inline
import RBNF.CodeGen
import RBNF.FrontEnd
import RBNF (parserGen)
import RBNF.Serialization

import RBNF.IRs.Marisa
import RBNF.IRs.MarisaLibrary
import RBNF.IRs.IRTrans
import RBNF.BackEnds.TargetGen (emit, PythonBackEnd)

import Prelude hiding (writeFile)
import Data.Foldable
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO (writeFile)
import Data.Aeson
import Data.Text
import Control.Lens
import Control.Monad.State
import Control.Applicative ((<|>))


import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Text.Lazy.IO as T
import           Data.Text.Prettyprint.Doc

import Control.Arrow

infix 5 -->
infix 6 |=

number = "number"
negation = "-"
multiply = "*"

a |= b = CBind a b

a --> b = (a, b, Nothing)
parsers = CGrammar [
    "Atom"   --> CAlt [ CTerm number, CSeq [CTerm "(", CNonTerm "Mul", CTerm ")"]]
    , "Mul"    --> CAlt [
            CNonTerm "Atom",
            CSeq [ CNonTerm "Mul", CTerm multiply, CNonTerm "Atom"]
        ]
    ]

bnf = "S ::= ['+' | '-'] <decimals> '.' <decimals>;"

test1 = do
    putStrLn ""
    -- for_ (M.toList $ followSet $ mkGrammar parsers) $ \(a, b) ->
    --         putStr a >> putStrLn ":" >> putStrLn (L.intercalate ", " $ map show b)
    let gbuilder = mkGrammar $ parsers
    let g = inline $ markedLeftRecur "START" gbuilder
    let ks = pGToSG  g
    let ps = view prods g
    let ms = buildGraph ks
    writeFile "./test.json" (encodeToLazyText ms)

test2 = do
    putStrLn ""
    let a = MKIf (dsl_int 1) (dsl_int 2) (dsl_int 3)
    print . seeMarisa $ a

test3 = do
    putStrLn ""
    let gbuilder = mkGrammar $ parsers
    let g = inline $ markedLeftRecur "START" gbuilder
    let ks = pGToSG  g
    let graph = buildGraph ks
    let dectrees = M.map decideFromLATree $ makeLATables 1 graph
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


    print . seeMarisa $ runToCode cfg $ codeGen c i

    putStrLn ""
    let s' = "Mul"
        i' = view ends graph M.! s'
    print . seeMarisa $ runToCode cfg $ codeGen c {isLeftRec = True} i'

-- def parse.Number(%state, %tokens)
-- var %off =  %tokens.offset
-- var .slot.0 =  %match_tk(%tokens, %tk_id("number"))
-- if %==(.slot.0, %null)
-- then %null
-- else var .slot.-1 =  %mk_ast("Number", tuple(.slot.0))
--      .slot.-1

test5  = T.writeFile "a.txt" $ dumpCG parsers

test6 = do
    let parsers = parseDoc bnf
    parsers <- case parsers of
        Left err -> error err
        Right (a, s) -> return a
    putStrLn ""
    let a = parserGen False True 1 False parsers
        py :: Marisa -> Doc PythonBackEnd
        py = emit
    print $ py a

test7 bnf = do
    putStrLn ""
    let parsers = parseDoc bnf
    parsers <- case parsers of
        Left err -> error err
        Right (a, s) -> return a

    let gbuilder = mkGrammar $  parsers
    let g = markedLeftRecur "START" gbuilder
    forM_ (M.toList $ view prods g) $ \(k, v) -> do
        print k
        forM_ v (\x -> putStrLn "===" >> print x)
        putStrLn ""
    let ks = pGToSG  g
    let ps = view prods g
    let ms = buildGraph ks
    writeFile "./test.json" (encodeToLazyText ms)
    let trees = M.map (id&&&decideFromLATree) $ makeLATables 2 ms
    forM_ (M.toList trees) $ \(i, (latrees, id3tree)) -> do
        putStrLn $ "======== Node" ++ show i ++ " || " ++ show (_nodes ms M.! i) ++ " ========"
        forM_ latrees $ putStrLn . dispLATree 0
        putStrLn "--- LA optimization:"
        putStrLn $ dispDecison 0 id3tree
        putStrLn ""

bnf2 = "START : <BOF> Type <EOF>;\nType         : TypeArrow;\nTypeArrow    : TypeApp ['->' TypeArrow];\nTypeApp      : [TypeApp] LitType;\nLitType      : '*';"

test8 bnf = do
    putStrLn ""
    let parsers = parseDoc bnf2
    parsers <- case parsers of
        Left err -> error err
        Right (a, s) -> return a
    let gbuilder = mkGrammar $  parsers
    let g = markedLeftRecur "START" gbuilder
    putStrLn "leftR:"
    forM_ (M.toList $ view leftR g) print
    putStrLn "prods:"
    forM_ (M.toList $ view prods g) print

main = test7 bnf2
