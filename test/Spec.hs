{-# LANGUAGE MonadComprehensions #-}

import RBNF.Grammar
import RBNF.Symbols
import RBNF.LeftRecur
import RBNF.Semantics
import RBNF.Graph
import RBNF.LookAHead
import RBNF.CodeGen
import RBNF (parserGen)
import RBNF.Serialization

import RBNF.IRs.Marisa
import RBNF.IRs.MarisaLibrary
import RBNF.IRs.Reimu
import RBNF.IRs.ReimuTyping
import RBNF.IRs.IRTrans
import RBNF.TypeSystem (RT)
import RBNF.BackEnds.TargetGen (emit, OCamlBackEnd)

import RSolve.Solver
import RSolve.PropLogic
import RSolve.MultiState

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
parsers = CGrammar [
    "Number"   --> CTerm number
    , "Factor" --> CAlt [
            CNonTerm "Number",
            CSeq [CTerm negation, "a" |= CNonTerm "Factor" ]
        ]
    , "Mul"    --> CAlt [
            CSeq [ CPred (MTerm "always_true"), CNonTerm "Factor" ],
            CSeq [ CNonTerm "Mul", CTerm multiply, CNonTerm "Factor"]
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
    let a = MKIf (dsl_int 1) (dsl_int 2) (dsl_int 3)
    print . seeMarisa $ a
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

test4 = do
    putStrLn ""
    let a = parserGen 1 False parsers
    -- print $ seeMarisa a
    -- let a =
    --       ADef (AName "parse.Number") [ABuiltin "state", ABuiltin "tokens"] $
    --           ABlock [
    --                 AAssign (ABuiltin "off") (AAttr (AVar $ ABuiltin "tokens") "offset")
    --             --   , ACall (AVar $ ABuiltin "tk_id") [AStr "number"]
    --             --   , ACall (AVar $ ABuiltin "match_tk") [AVar $ ABuiltin "tokens", AInt 0]
    --             --   , ACall (AVar $ ABuiltin "match_tk") [AVar $ ABuiltin "tokens", ACall (AVar $ ABuiltin "tk_id") [AStr "number"]]
    --                 -- AAssign (AName ".slot.0")
    --             --   , AVar (ABuiltin "off")
    --           ]
    let bs = irTransform a :: [Reimu RT]
    -- ($ bs) (print . pretty)
        ocaml :: Reimu RT -> Doc OCamlBackEnd
        ocaml = emit
    forM_ bs $ print . seeReimu
    -- putStrLn "\n BIR with declarations:"
    -- printBIR 80 $ bWithDecl
    -- let env = emptyTCEnv emptyTInfo
    -- let res = flip runMS env $ do
    --         basicTCEnv True
    --         bWithAnnotated <- tc bWithDecl
    --         constrs <- getsMS $ view (ext . constr)
    --         let dnfs = unionEquations $ forM_ constrs assert
    --             ms =  flip L.map dnfs $ \dnf ->
    --                         forM dnf solve
    --             alts = case ms of
    --                     [] -> error "emm"
    --                     x:xs -> L.foldl (<|>) x xs
    --         alts
    --         bTyped <- pruneTypedBIR bWithAnnotated
    --         return $ typedBIRToDoc bTyped
    -- putStrLn "\npossible typed BIR:"

    -- -- -- print $ L.length res
    -- forM_ res $ \(doc, _) ->
    --     print doc

test5  = T.writeFile "a.txt" $ dumpCG parsers
main = test4