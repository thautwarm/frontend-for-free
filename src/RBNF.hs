module RBNF
where

import RBNF.Utils
import RBNF.Symbols
import RBNF.Grammar
import RBNF.LeftRecur
import RBNF.Semantics
import RBNF.LookAHead
import RBNF.Graph
import RBNF.CodeGen
import RBNF.CodeGenIRs.A

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

parserGen :: Int -> Bool -> CGrammar -> AIR
parserGen k withTrace cg = ABlock $ lrCodes ++ nonLRCodes
    where
        graph      = parsingGraph cg
        decisions  = M.map decideId3FromLATree $ makeLATables k graph
        c = CompilationInfo {
            graph     = graph
            , decisions = decisions
            , withTrace = withTrace
            , isLeftRec = False
           }
        startNodeIds = M.toList . view starts $ graph
        endsNodeIds  = M.toList . view ends   $ graph
        nonLRCodes   = startNodeIds >>= \(s, i) ->
                let cfg = emptyCFG s
                in  runToCodeSuite cfg $ codeGen c i
        lrCodes      =
            endsNodeIds >>= \(s, i) ->
                case kind $ view nodes graph M.! i of
                    LeftRecur ->
                        let cfg = emptyCFG s
                        in  runToCodeSuite cfg $ codeGen c {isLeftRec = True} i
                    _ -> []

parsingGraph :: CGrammar -> Graph
parsingGraph =
    buildGraph .
    pGToSG .
    markedLeftRecur .
    mkGrammar