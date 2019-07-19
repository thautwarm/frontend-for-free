{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module RBNF.LookAHead where

import RBNF.Graph
import RBNF.Semantics
import RBNF.Symbols (Case)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Arrow
import Control.Lens (over, view, Lens', makeLenses)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

type Map = M.Map

data Travel = Travel { par :: Maybe Travel , cur :: Int }
    deriving (Eq, Ord, Show)

data LAEdge = LAShift Case | LAReduce
    deriving (Eq, Ord, Show)
data LATree a
    = LA1 (Map LAEdge (LATree a))
    | LAEnd [a]
    deriving (Eq, Ord, Show, Functor)

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
                        trvls   = [parent {cur = i} | i <- view nextBrs parNode]
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
        nextIndices  = view nextBrs curNode

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
        NZ     -> LA1 . M.mapKeys LAShift $ M.map LAEnd xs
        NS n'  -> LA1 . M.mapKeys LAShift $ M.map (mergeLATrees . L.nub . map nextDec1) xs

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
mergeLATrees las = LA1 cases
    where
        frec :: [LATree a] -> [(LAEdge, LATree a)]
        frec  = \case
            []        -> []
            LA1 mp:xs -> (M.toList mp ++) $ frec xs
            a:xs      -> ((LAReduce, a):) $ frec xs

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
        nexts = view nextBrs $ view nodes graph M.! idx
        trvls = [root {par = Just root, cur=next} | next <- nexts]
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
        | L.length (view nextBrs node) > 1 ->
        modify $ M.insert idx (lookAHeadRoot k graph idx)
    _ -> return ()