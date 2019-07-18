{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RBNF.Graph where


import RBNF.Symbols
import RBNF.Grammar
import RBNF.Semantics
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens (over, view, Lens', makeLenses)

data NodeKind =
      NShift Shiftable
    | NProc {
          codes::[(Int, IR, ProgKind)]
        , bounds :: Map String Int
       }
    | DoNothing

data Next = Next {
      optional :: Bool
    , to :: Int
  }

data Node =
    Node {
          kind :: NodeKind
        , _nexts :: [Next]
    }

data Graph = Graph {
        _nodes  :: V.Vector Node
      , _starts :: M.Map String Int
      , _ends   :: M.Map String Int
    }

makeLenses ''Node
makeLenses ''Graph

initGraph syms =
    let n = length syms
        nodes' = V.fromList
                [Node DoNothing [] | s <- [1 .. (2*n)]]
        starts' = M.fromList [(s, 2*i+1) |(s, i) <- zip syms [0..(n-1)]]
        ends'   = M.fromList [(s, 2*i) |(s, i) <- zip syms [0..(n-1)]]
    in Graph nodes' starts' ends'

-- pPRuleReg :: PRule ->
-- buildGraph_ :: String -> [PRule] -> StateT Graph (Reader (Grammar P)) ()
-- buildGraph_ sym prules =
--     do
--     start <- gets $ (M.! sym) . view starts
--     a <- lift $ asks (view prods)
--     error ""