{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module RBNF.LookAHead where

import Debug.Trace

import RBNF.Graph
import RBNF.Semantics
import RBNF.Symbols (Case)
import RBNF.Grammar (groupBy)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Arrow
import Control.Lens (over, view, Lens', makeLenses)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V
import qualified Data.Set as S

type Map = M.Map

data Travel = Travel { par :: Maybe Travel , cur :: Int }
    deriving (Eq, Ord, Show)

data LAEdge = LAShift Case | LAReduce
    deriving (Eq, Ord, Show)
data LATree a
    = LA1 (Map LAEdge (LATree a))
    | LAEnd [a]
    deriving (Eq, Ord, Show, Functor)

dispLATree :: Show a => Int -> LATree a -> String
dispLATree i = \case
    LAEnd xs -> indent i $ show xs
    LA1 m    -> L.intercalate "\n" $
                flip map (M.toList m) $
                    \(case', la) ->
                    indent i $ (
                        show case' ++
                        "\n" ++
                        dispLATree (i + 4) la)
data Coro a o r
    = Coro { fp :: a -> Either r (o, Coro a o r) }

type Generator a o = MaybeT (State (Coro a o ())) o

yield :: a -> Generator a o
yield a = do
    fp <- lift $ gets fp
    let either = fp a
    case either of
        Left () -> mzero
        Right (o, coro) -> do
            lift $ put coro
            return o


getNode :: Int -> Reader Graph Node
getNode i = asks $ (M.! i) . view nodes

getStartIdx :: String -> Reader Graph Int
getStartIdx i = asks $ (M.! i) . view starts


uniqueCat :: Eq a => [a] -> [a] -> [a]
uniqueCat a b = L.nub $ a ++ b

nextBrs :: Node -> [Int]
nextBrs = view followed

next1 :: Graph -> Travel -> Map Case [Travel]
next1 graph travel =
    case kind curNode of
        NEntity (ENonTerm s) ->
            let idx      = startIndices M.! s
                descTrvl = Travel (Just travel) idx
            in frec descTrvl
        NEntity (ETerm c) ->
            let newTrvls = [travel {cur = nextIdx} | nextIdx <- nextIndices]
            in M.singleton c $ newTrvls
        _ ->
            case (nextIndices, par travel) of
                ([], Nothing) -> M.empty
                ([], Just parent) ->
                    let parNode = nodeStore M.! cur parent
                        trvls   = [parent {cur = i} | i <- nextBrs parNode]
                    in M.unionsWith uniqueCat $ map frec trvls
                (xs, _) ->
                    let trvls = [travel {cur = i} | i <- nextIndices]
                    in M.unionsWith uniqueCat $ map frec trvls
    where
        frec         = next1 graph
        endIndices   = view ends graph
        startIndices = view starts graph
        nodeStore    = view nodes graph

        curIdx       = cur travel
        curNode      = nodeStore M.! curIdx
        nextIndices  = nextBrs curNode

isRec Travel {cur, par=Just par} = frec cur par
        where
            frec i Travel {cur, par}
             | i == cur = True
             | otherwise = case par of
                Nothing -> False
                Just par -> frec i par

data Nat' = NZ | NS Nat'
nextK :: Graph -> Travel -> Nat' -> LATree Travel
nextK graph trvl n =
    case n of
        _ | M.null xs -> LAEnd [trvl]
        NZ     -> LA1 . M.mapKeys LAShift $
                  M.map LAEnd xs
        NS n'  -> LA1 . M.mapKeys LAShift $
                  M.map (mergeLATrees . L.nub . map nextDec1) xs

            where nextDec1 :: Travel -> LATree Travel
                  nextDec1 trvl =
                    let n'' = case kind $ view nodes graph M.! cur trvl of
                            NEntity (ENonTerm _) -> n'
                            -- avoid infinite recursing for productions
                            -- referring no nonterminals other than itself.
                            Stop                 -> n'
                            _                    -> n

                    in nextK graph trvl n''
    where xs = next1 graph trvl

mergeLATrees ::  [LATree a] -> LATree a

mergeLATrees [] = error "invalid"
mergeLATrees [a] = a
mergeLATrees las = LA1 cases
    where
        frec :: [LATree a] -> [(LAEdge, LATree a)]
        frec  = \case
            []        -> []
            LA1 mp:xs -> (M.toList mp ++) $ frec xs
            a:xs      -> (LAReduce, a):frec xs

        cases = M.map mergeLATrees       $
                M.fromListWith (++)      $
                map (fst &&& pure . snd) $ frec las

intToNat :: Int -> Nat'
intToNat i
    | i < 0 = error "invalid" -- TODO
    | otherwise = intToNat' i
    where intToNat' = \case
            0 -> NZ
            n -> NS $ intToNat' $ n-1

type LANum = Int
lookAHeadRoot :: LANum -> Graph -> Int -> LATree Int
lookAHeadRoot k graph idx =
    let root  = Travel {cur=idx, par=Nothing}
        nexts = nextBrs $ view nodes graph M.! idx
        trvls = [root {par=Nothing, cur=next} | next <- nexts]
        n     = intToNat k
    in
    mergeLATrees [cur trvl <$ nextK graph trvl n | trvl <- trvls]

makeLATables :: LANum -> Graph -> Map Int (LATree Int)
makeLATables k graph =
    flip execState M.empty $
    forM_ (M.toList        $
    view nodes graph)      $
    \case
    (idx, node)
        | L.length (nextBrs node) > 1 ->
        modify $ M.insert idx (lookAHeadRoot k graph idx)
    _ -> return ()


flattenLATree :: LATree a -> [([LAEdge], a)]
flattenLATree = \case
    LAEnd xs -> [([], x) | x <- xs]
    LA1 m     ->
        let groups = M.toList m
        in flip concatMap groups $ \(case', la) ->
            let xs = flattenLATree la
            in flip map xs $ ((case':) . fst) &&& snd

data ID3Decision elt cls
    = ID3Split Int [(elt, ID3Decision elt cls)]
    | ID3Leaf [cls]
    deriving (Show, Eq, Ord)

dispID3Tree :: (Show cls, Show elt) => Int -> ID3Decision elt cls -> String
dispID3Tree i = \case
    ID3Leaf xs -> show xs
    ID3Split n xs -> head ++ "\n" ++ body
        where nextI = i + 4
              head  = "case elts[" ++ show n ++ "]"
              body  = L.intercalate "\n" . flip map xs $
                        \(elt, tree) ->
                            indent nextI (show elt) ++
                            " => " ++
                            dispID3Tree nextI tree

type Offsets = [Int]
type Numbers = [Int]
type PathsOfElements elt = V.Vector (V.Vector elt)
type States cls = V.Vector cls
data DecisionProcess
    = DP {
        offsets :: [Int],
        numbers :: [Int]
    }

data ArgMax a = ArgMax {idx:: Int, val:: a}
instance Eq a => Eq (ArgMax a) where
    a == b = val a == val b

instance Ord a => Ord (ArgMax a) where
    a <= b = val a <= val b

argmaxWithVal :: (Ord a) => [a] -> (Int, a)
argmaxWithVal xs =
    let ArgMax {idx, val} = maximum $ zipWith ArgMax [0..] xs
    in (idx, val)

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
        distinctness xs = 1.0 / (sum [lengthf (L.nub x) * lengthf x | x <- xs]) * sum (map lengthf xs)

decideID3 :: (Ord elt, Ord cls) => StateT DecisionProcess (Reader (States cls, PathsOfElements elt)) (ID3Decision elt cls)
decideID3 = do
    cur <- get
    env@(states, paths) <- lift ask
    let minLen       = minimum $ V.map V.length paths
        validOffsets = takeWhile (< minLen) $ offsets cur
        states'      = V.toList states
        nums         = L.nub . map (states V.!) $ numbers cur
    if L.null validOffsets then
        return $ ID3Leaf nums
    else let
            clsfInfos    = flip map validOffsets $ \j ->
                                classifInfo states'
                                [paths V.! i V.! j | i <- numbers cur]
            (nth, maxI)  = argmaxWithVal clsfInfos
            split        = M.toList . groupBy (\i -> paths V.! i V.! nth) $ numbers cur
            nextOffsets  = L.delete nth $ offsets cur

            recurse = \case
                [] -> []
                (elt, nums):xs ->
                    case L.nub nums of
                        [num] -> (elt, ID3Leaf [states V.! num]):tl
                        ns    ->
                            let hd = flip runReader env $
                                    evalStateT decideID3 cur {offsets = nextOffsets, numbers = ns}
                            in  (elt, hd):tl
                    where tl = recurse xs
        in return $
            if maxI == 0.0 || validOffsets == [] || L.length nums == 1
            then ID3Leaf nums
            else ID3Split nth $ recurse split

decideId3FromLATree :: Ord cls => LATree cls -> ID3Decision LAEdge cls
decideId3FromLATree tree =
    let (paths_src, states_src) = unzip $ flattenLATree tree
        paths  = V.fromList [V.fromList row | row <- paths_src]
        states = V.fromList states_src
        numbers = [0.. V.length paths - 1]
        offsets = [0..maximum (V.map V.length paths) - 1]
        env = (states, paths)
        dp = DP {offsets = offsets, numbers = numbers}
    in flip runReader env $
        evalStateT decideID3 dp