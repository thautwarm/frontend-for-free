{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RBNF.Graph where

import RBNF.Symbols
import RBNF.Grammar
import RBNF.Semantics
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Control.Lens (over, view, Lens', makeLenses)

import Debug.Trace

data NodeKind =
      NEntity Entity
    | NProc [(Int, IR)] SlotIdx
    | Stop
    | Start
    | DoNothing
    deriving (Show)

data Node =
    Node {
          kind :: NodeKind
        , _nextBrs :: [Int]
        , _elseBr  :: Maybe Int
    }
    deriving (Show)

makeLenses ''Node

data Graph = Graph {
        _nodes  :: M.Map Int Node
      , _starts :: M.Map String Int
      , _ends   :: M.Map String Int
    }
    deriving (Show)

makeLenses ''Graph

initGraph syms =
    let n        = length syms
        initNo k = Node k [] Nothing
        starts'  = [(s, 2*i+1) |(s, i) <- zip syms [0..(n-1)]]
        ends'    = [(s, 2*i) |(s, i) <- zip syms [0..(n-1)]]
        nodes'   = [(i, initNo Start)   | (_, i) <- starts']
                   ++ [(i, initNo Stop) | (_, i) <- ends']

        map_ :: Ord k => [(k, a)] -> Map k a
        map_     = M.fromList
    in Graph (map_ nodes') (map_ starts') (map_ ends')

newNode :: NodeKind -> State Graph Int
newNode kind = do
  nodes' <- gets $ view nodes
  let refIdx   = M.size nodes'
      newNode' = Node kind [] Nothing
  modify $ over nodes (M.insert refIdx newNode')
  return refIdx

adjustHd :: Int -> (Node -> Node) -> State Graph ()
adjustHd hdIdx by = modify $ over nodes (M.adjust by hdIdx)

buildNext :: Maybe String -> Int -> [Seman] -> State Graph [Int]
buildNext lr headIdx semans =
  case reachFinals of
    []  -> ms
    [x] -> do
        let codes' = view prog x
            retVal = ret x

        newNodeIdx' <- newNode $ NProc codes' retVal
        adjustHd headIdx $ over elseBr (const $ Just newNodeIdx')
        (newNodeIdx':) <$> ms
    _ -> error "invalid syntax" -- TODO: invalid syntax
  where
      reachFinals = fst partitioned
      others      = snd partitioned
      partitioned = L.partition ((== []) . view route) semans

      groups      =
        let tuples    :: [(Entity, Seman)]
            tuples    = map extractHd others
            extractHd :: Seman -> (Entity, Seman)
            extractHd = L.head . view route &&& over route L.tail
        in  M.map (map snd) $ groupBy fst tuples

      -- m is a common monadic operation.

      mGBase lr (hd, semans) = do
        newNodeIdx' <- newNode (NEntity hd)
        indices     <- buildNext lr newNodeIdx' semans
        adjustHd headIdx $ over nextBrs (newNodeIdx':)
        return indices

      mG = case lr of
            x@(Just s) -> \case
                          (ENonTerm s', semans)
                            | s == s' -> buildNext Nothing headIdx semans
                          a -> mGBase lr a
            _ -> mGBase Nothing

      ms  = concat <$> forM (M.toList groups) mG

buildNonTerm :: (String, [Seman]) -> State Graph ()
buildNonTerm (sym, semans) | not $ L.null semans = do
  startIdx <- gets $ (M.! sym) . view starts
  endIdx   <- gets $ (M.! sym) . view ends
  indices  <- buildNext Nothing startIdx semans
  forM_ indices $ \i ->
    adjustHd i $ over elseBr (const $ Just endIdx)

buildLeftR :: (String, [Seman]) -> State Graph ()
buildLeftR (sym, semans) | not $ L.null semans = do
  endIdx   <- gets $ (M.! sym) . view ends
  indices  <- buildNext (Just sym) endIdx semans
  forM_ indices $ \i ->
    adjustHd i $ over elseBr (const $ Just endIdx)

buildGraph :: Grammar Seman -> Graph
buildGraph g =
    flip execState (initGraph syms) $ do
        mapM_ buildNonTerm $ M.toList prods'
        mapM_ buildLeftR   $ leftR'
    where anyEntity = filter (not . L.null . snd)
          prods' = view prods g
          leftR' = anyEntity $ M.toList $ view leftR g
          syms   = M.keys prods'