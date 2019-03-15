{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RBNF.GraphAnalysis.IRs where
import RBNF.Semantics
import qualified Data.Map as M

-- Each non-terminal may have multiple branches.
type ExpandedGraph = M.Map String [ExpandedNodes]
type ExpandedNodes = [ExpandedNode]
data ExpandedNode where
    EpsE     :: ExpandedNode
    Rep1E    :: ExpandedNode -> Range -> ExpandedNode
    Rep2E    :: [ExpandedNode] -> Range -> ExpandedNode
    RefE     :: String -> ExpandedNode
    LitE     :: Lexer -> ExpandedNode
    PackE    :: String -> Int -> ExpandedNode
    deriving (Eq, Ord, Show)

type ReducedGraph = M.Map String ReducedNode

type ReducedGraphStore = M.Map Int ReducedNode

data NodeInfo = RepI Range | RefI String | LitI Lexer | PackI String Int

data ReducedNode =
      Epsilon
    | ReducedNode {parents :: [Int], children :: [Int], info :: NodeInfo}
