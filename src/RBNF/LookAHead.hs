-- | Building Lookahead trees from parsing graphs
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-07-18
-- License: BSD-3-clause
{-# LANGUAGE RankNTypes #-}
module RBNF.LookAHead where

import           RBNF.Graph
import           RBNF.Semantics
import           RBNF.Symbols
import           RBNF.Grammar
import           RBNF.Utils

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Arrow
import           Data.Foldable                  ( minimumBy )
import           Data.Ord                       ( comparing )
import           Debug.Trace                    ( trace )

import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Maybe                    as Maybe
import qualified Data.Vector                   as V
import qualified Data.Set                      as S

data Travel = Travel { par :: Maybe Travel , cur :: Int }
    deriving (Eq, Ord, Show)


type LAEdge = String
data LATree a
    = LA1 LAEdge [LATree a]
    | LAEnd [a]
    deriving (Eq, Ord, Show, Functor)

newtype LAForest a = LAForest [LATree a]
    deriving (Eq, Ord, Show, Functor)

dispLATree :: Show a => Int -> LATree a -> String
dispLATree i = \case
    LAEnd xs    -> indent i $ "end: " ++ show xs
    LA1 edge xs -> indent i ("case " ++ edge) ++ "\n" ++ nest
        where nest = unlines $ map (dispLATree (i + 4)) xs

dispLATrees :: Show a => Int -> [LATree a] -> String
dispLATrees i xs = unlines $ map (dispLATree i) xs

mapToTrees :: Map LAEdge [LATree a] -> [LATree a]
mapToTrees m = [ LA1 edge trees | (edge, trees) <- M.toList m ]


getNode :: Int -> Reader Graph Node
getNode i = asks $ (M.! i) . view nodes

getStartIdx :: String -> Reader Graph Int
getStartIdx i = asks $ (M.! i) . view starts


uniqueCat :: Eq a => [a] -> [a] -> [a]
uniqueCat a b = L.nub $ a ++ b

nextBrs :: Node -> [Int]
nextBrs = view followed

data Next1
    = Next1 {
        foundStop :: Bool
      , nextTravel :: Travel
    }
    deriving (Eq, Ord, Show)

next1 :: Graph -> Travel -> (Bool, Map String [Next1])
next1 graph travel = case kind curNode of
    NEntity (ENonTerm s) ->
        let idx      = startIndices M.! s
            descTrvl = Travel (Just travel) idx
            (a, b)   = frec descTrvl
        in  (a, M.map (map stop) $ b)
    NEntity (ETerm c) ->
        let
            next1s =
                [ Next1 False travel { cur = nextIdx }
                | nextIdx <- nextIndices
                ]
        in  (False, M.singleton c next1s)
    ent -> flip detectStop ent $ case (nextIndices, par travel) of
        ([], Nothing) -> (True, M.empty)
        ([], Just parent) ->
            let parNode  = nodeStore M.! cur parent
                trvls    = [ parent { cur = i } | i <- nextBrs parNode ]
                (as, bs) = unzip $ map frec trvls
            in  (or as, M.unionsWith uniqueCat bs)
        (xs, _) ->
            let trvls    = [ travel { cur = i } | i <- nextIndices ]
                (as, bs) = unzip $ map frec trvls
            in  (or as, M.unionsWith uniqueCat bs)
  where
    frec         = next1 graph
    endIndices   = view ends graph
    startIndices = view starts graph
    nodeStore    = view nodes graph

    curIdx       = cur travel
    curNode      = nodeStore M.! curIdx
    nextIndices  = nextBrs curNode

    stop x = x { foundStop = True }
    detectStop (a, b) = \case
        Stop      -> (True, M.map (map stop) b)
        LeftRecur -> (True, M.map (map stop) b)
        _         -> (a, b)

data Nat' = NZ | NS Nat'
nextK :: Graph -> Travel -> Nat' -> [LATree Travel]
nextK graph trvl n | stopable  = LAEnd [trvl] : laTrees
                   | otherwise = laTrees
  where
    (stopable, xs) = next1 graph trvl
    xsToTrees f = mapToTrees . M.map f $ xs
    laTrees = case n of
        _ | M.null xs -> [LAEnd [trvl]]
        NZ            -> xsToTrees $ pure . LAEnd . map nextTravel
        NS n'         -> xsToTrees $ concatMap nextDec1
          where
            nextDec1 :: Next1 -> [LATree Travel]
            nextDec1 Next1 { foundStop, nextTravel } =
                nextK graph nextTravel $ if foundStop then n' else n

mergeLATrees :: Eq a => [LATree a] -> [LATree a]
mergeLATrees []  = error "invalid"
mergeLATrees [a] = [a]
mergeLATrees las = cases
  where
    frec :: [LATree a] -> State [a] (Map LAEdge [LATree a])
    frec = \case
        []               -> pure M.empty
        LA1 edge ms : xs -> M.insertWith (++) edge ms <$> frec xs
        LAEnd sts   : xs -> modify (++ sts) >> frec xs
    (trees, ends) = runState (frec las) []
    nonEnds       = mapToTrees (M.map (mergeLATrees . L.nub) trees)
    cases | L.null ends = nonEnds
          | otherwise   = LAEnd (L.nub ends) : nonEnds

intToNat :: Int -> Nat'
intToNat i | i < 0     = error "invalid"
           | -- TODO
             otherwise = intToNat' i
  where
    intToNat' = \case
        0 -> NZ
        n -> NS $ intToNat' $ n - 1

type LANum = Int
lookAHeadRoot :: LANum -> Graph -> Int -> [LATree Int]
lookAHeadRoot k graph idx =
    let
        root    = Travel { cur = idx, par = Nothing }
        nexts   = nextBrs $ view nodes graph M.! idx
        trvls   = [ root { par = Nothing, cur = next } | next <- nexts ]
        n       = intToNat k
        forests = [ cur trvl <$ LAForest (nextK graph trvl n) | trvl <- trvls ]
        latrees = L.nub . concat $ [ trees | LAForest trees <- forests ]
    in
        mergeLATrees latrees


makeLATables :: LANum -> Graph -> Map Int [LATree Int]
makeLATables k graph =
    flip execState M.empty $ forM_ (M.toList $ view nodes graph) $ \case
        (idx, node) | L.length (nextBrs node) > 1 ->
            modify $ M.insert idx (lookAHeadRoot k graph idx)
        _ -> return ()


showLATreePaths :: Show a => [([LAEdge], a)] -> String
showLATreePaths [] = []
showLATreePaths ((path, a) : xs) =
    show path ++ " -> " ++ show a ++ "\n" ++ showLATreePaths xs

flattenLATrees :: [LATree a] -> [([LAEdge], a)]
flattenLATrees = \case
    []               -> []
    LAEnd xs    : tl -> [ ([], x) | x <- xs ] ++ flattenLATrees tl
    LA1 edge xs : tl -> recurse ++ flattenLATrees tl
      where
        recurse =
            flattenLATrees xs >>= \(edges, state) -> pure (edge : edges, state)

data ID3Decision elt cls
    = ID3Split Int [(elt, ID3Decision elt cls)]
    | ID3Optional cls (ID3Decision elt cls)
    | ID3Leaf [cls]
    deriving (Show, Eq, Ord)

dispID3Tree :: (Show cls, Show elt) => Int -> ID3Decision elt cls -> String
dispID3Tree i = \case
    ID3Leaf xs       -> show xs
    ID3Optional x xs ->
        "optional " ++ show x ++ "\n" ++ (indent i $ dispID3Tree i xs)
    ID3Split n xs -> head ++ "\n" ++ body
      where
        nextI = i + 4
        head  = "case elts[" ++ show n ++ "]"
        body  = L.intercalate "\n" . flip map xs $ \(elt, tree) ->
            indent nextI (show elt) ++ " => " ++ dispID3Tree nextI tree

data DecisionProcess elt cls
    = DP {
        offsets :: [Int],
        transi  :: [(V.Vector elt,  cls)]
    }

data ArgMax a = ArgMax {idx:: Int, val:: a}
instance Eq a => Eq (ArgMax a) where
    a == b = val a == val b

instance Ord a => Ord (ArgMax a) where
    a <= b = val a <= val b

argmaxWithVal :: (Ord a) => [a] -> (Int, a)
argmaxWithVal xs =
    let ArgMax { idx, val } = maximum $ zipWith ArgMax [0 ..] xs in (idx, val)

-- f([1, 1, 1, ...], [1, 2, 3, ...]) = 0
-- {1: [1, 2, 3, 4, ...]}
-- f([1, 2, 3, ...], [1, 2, 3, ...]) = 1
-- {1:[1], 2:[2], 3:[3], ...}

classifInfo :: (Eq cls, Ord elt) => [cls] -> [elt] -> Double
classifInfo clses elts =
    let separated = map (map snd) $ M.elems $ groupBy fst $ zip elts clses
    in  distinctness separated
  where
    lengthf = fromIntegral . length
    distinctness xs =
        1.0 / sum [ lengthf (L.nub x) * lengthf x | x <- xs ] * sum
            (map lengthf xs)

decideID3
    :: (Ord elt, Ord cls) =>
    DecisionProcess elt cls
    -> ID3Decision elt cls

decideID3 cur@DP {offsets=curOffsets, transi = transi@(unzip -> (paths, states))}
    | V.null validOffsets
    = let state       = states !! minIdx
      in case deleteAt minIdx transi of
            [] -> ID3Leaf [state]
            transi  -> ID3Optional state $ decideID3 cur {transi}
    | otherwise
    = let
          score j = classifInfo states [ path V.! j | path <- paths ]
          -- calculate i= 1 .. n for lookahead(n), select the best split node.
          clsfInfos   = V.map score validOffsets
          maxInfoInd  = V.maxIndex clsfInfos
          nth         = validOffsets V.! maxInfoInd
          maxInfo     = clsfInfos V.! maxInfoInd
          split       = M.toList . groupBy (\(path, state) -> path V.! nth) $ transi
          nextOffsets = L.delete nth curOffsets

          recurse     = \case
              []                 -> []
              (elt, transi) : xs -> case L.nub $ map snd transi of
                  [state] -> (elt, ID3Leaf [state]) : tl
                  _ ->
                      let
                          hd = decideID3 cur { offsets = nextOffsets, transi}
                      in  (elt, hd) : tl
                  where tl = recurse xs
      in if maxInfo == 0.0 || L.null nextOffsets
         then case transi of
                [(path, state)] -> ID3Split nth [(path V.! nth, ID3Leaf [state])]
                _ -> ID3Leaf $ map snd transi
         else ID3Split nth $ recurse split
  where
    lengths      = V.fromList $ map V.length paths
    minIdx       = V.minIndex lengths
    minLen       = lengths V.! minIdx
    validOffsets = V.fromList $ takeWhile (< minLen) curOffsets

decideId3FromLATree
    :: Ord cls => [LATree cls] -> ID3Decision LAEdge cls
decideId3FromLATree trees =
    let transi  = map (V.fromList . fst &&& snd) $ flattenLATrees trees
        offsets = [0 .. maximum [V.length path | (path, _) <- transi] - 1]
        dp      = DP {offsets,  transi}
    in decideID3 dp

data Decision elt cls =
      NDSplit Int [cls] [(elt, Decision elt cls)]
    | NDLeaf [cls]

normalizeDecision :: (Eq cls) => ID3Decision elt cls -> Decision elt cls
normalizeDecision = \case
    ID3Optional x nest ->
        case normalizeDecision nest of
            NDSplit i clses xs -> NDSplit i (x:clses) xs
            NDLeaf clses       -> NDLeaf (x:clses)
    ID3Split i xs -> NDSplit i [] $ map (fst &&& normalizeDecision . snd) xs
    ID3Leaf xs    -> NDLeaf xs

decideFromLATree :: Ord cls => [LATree cls] -> Decision LAEdge cls
decideFromLATree = normalizeDecision . decideId3FromLATree

dispDecison :: (Show cls, Show elt) => Int -> Decision elt cls -> String
dispDecison i = \case
    NDLeaf xs  -> show xs
    NDSplit n clses xs -> optionals ++ head ++ "\n" ++ body
      where
        nextI = i + 4
        optionals = case clses of
            [] -> ""
            _  -> "optional: " ++ show clses ++ "\n" ++ indent i ""
        head  = "case elts[" ++ show n ++ "]"
        body  = unlines . flip map xs $ \(elt, tree) ->
            indent nextI (show elt) ++ " => " ++ dispDecison nextI tree