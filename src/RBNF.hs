module RBNF
where

import RBNF.Utils
import RBNF.Symbols
import RBNF.Grammar
import RBNF.LeftRecur
import RBNF.Inline
import RBNF.Semantics
import RBNF.LookAHead
import RBNF.Graph
import RBNF.CodeGen
import RBNF.IRs.Marisa
import RBNF.IRs.MarisaHeader

import RBNF.Serialization
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as T
-- (writeFile)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

parserGen :: Bool -> Int -> Bool -> CGrammar -> Marisa
parserGen doInline k withTrace cg = addHeader withTrace . MKBlock $ lrCodes ++ nonLRCodes
    where
        graph      = parsingGraph doInline cg
        decisions  = M.map decideFromLATree $ makeLATables k graph
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

parsingGraph :: Bool -> CGrammar -> Graph
parsingGraph doInline =
    buildGraph .
    pGToSG .
    (if doInline then inline else id) .
    markedLeftRecur .
    mkGrammar

graphToJSON :: String -> Graph -> IO ()
graphToJSON path ms = T.writeFile path (encodeToLazyText ms)