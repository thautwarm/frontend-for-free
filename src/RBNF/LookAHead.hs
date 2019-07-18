{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
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

data Travel = Travel { par :: Maybe Travel , cur :: Int }

data LATree
    = LA1 [(Case, [LATree])]
    | LAPending Travel
    | LAEnd

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


next1 :: Graph -> Travel -> [(Case, [Travel])]
next1 graph travel =
    case kind curNode of
        NEntity (ENonTerm s) ->
            let idx      = startIndices M.! s
                descTrvl = Travel (Just travel) idx
            in frec descTrvl
        NEntity (ETerm c) ->
            let newTrvls = [travel {cur = nextIdx} | nextIdx <- nextIndices]
            in [(c, newTrvls)]
        _ ->
            case (nextIndices, par travel) of
                ([], Nothing) -> []
                ([], Just parent) ->
                    let parNode = nodeStore M.! cur parent
                        trvls   = [parent {cur = i} | i <- view nextBrs parNode]
                    in concatMap frec trvls
                (xs, _) ->
                    let trvls = [travel {cur = i} | i <- nextIndices]
                    in concatMap frec trvls
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
nextK :: Graph -> Travel -> Nat' -> LATree
nextK graph trvl n =
    let xs = next1 graph trvl in
    case (xs, n) of
        ([], _)   -> LAEnd
        (xs, NZ)  -> LA1 $ map (fst &&& (map LAPending) . snd) xs
        (xs, NS n')  ->
            error ""
            -- flip map xs $ \(case', trvls) -> (case', map (flip (nextK graph) n') trvls)

nextCoro :: Generator Int [LATree]
nextCoro = error ""



