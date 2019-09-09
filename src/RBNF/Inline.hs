
module RBNF.Inline where
import           RBNF.Symbols
import           RBNF.Grammar
import           RBNF.Utils
import           Debug.Trace
import qualified Data.Map                      as M

inline :: Grammar [P] -> Grammar [P]
inline g = Grammar { _prods = inlineProds inlinedNLeftRs productions
                   , _leftR = inlineProds inlinedNLeftRs leftRs
                   }
  where
    productions    = view prods g
    leftRs         = M.filter (not . null) $ view leftR g
    nLeftRs        = M.difference productions leftRs
    inlinedNLeftRs = inlineProds nLeftRs nLeftRs
    inlineProd :: Map String [[P]] -> String -> [P] -> [[P]]
    inlineProd nLeftRs me rule =
        let letBlock :: String -> [P] -> [P]
            letBlock s xs = PPushScope s : xs ++ [PPopScope s]
            inlineP :: P -> [[P]]
            inlineP = \case
                x@(PNonTerm s) | s /= me && s `M.member` nLeftRs ->
                    map (letBlock s) (nLeftRs M.! s)
                x -> [[x]]
        in  map concat $ mapM inlineP rule
    inlineProds :: Map String [[P]] -> Map String [[P]] -> Map String [[P]]
    inlineProds nLeftRs m = M.fromList
        [ (me, concatMap (inlineProd nLeftRs me) rules)
        | (me, rules) <- M.toList m
        ]
