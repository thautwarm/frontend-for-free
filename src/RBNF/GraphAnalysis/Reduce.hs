{-# LANGUAGE LambdaCase #-}
module RBNF.GraphAnalysis.Reduce where
import RBNF.Semantics
import RBNF.GraphAnalysis.IRs

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Arrow

groupBy key = M.fromListWith (++) . map (key &&& pure)

groupNodes :: [ExpandedNodes] ->  M.Map ExpandedNode [ExpandedNodes]
groupNodes [] = M.empty
groupNodes (x:xs) =
    let m     = groupNodes xs
        hd:tl = x
        insert_f :: [ExpandedNodes] -> [ExpandedNodes] -> [ExpandedNodes]
        insert_f [one] old = one:old
    in M.insertWith insert_f hd [tl] m

unique [] = []
unique (x : xs) =
    unique' (S.singleton x) [x] xs
    where
        unique' occurred xs [] = reverse xs
        unique' occurred xs (hd:tl) =
            let
                (occurred', xs') =
                    if hd `notElem` occurred
                    then (S.insert hd occurred, hd:xs)
                    else (occurred, xs)
            in unique' occurred' xs' tl

reduce :: ExpandedGraph -> State ReducedGraph ()
reduce ctx =
    forM_ (M.toList ctx) reduceRoot
    where
        reduceRoot :: (String, [ExpandedNodes]) -> State ReducedGraph ()
        reduceRoot (rootName, branches) =
            let
                reduceEach :: [ExpandedNodes] -> State ReducedGraph ()
                reduceEach = \case
                    [] -> errorWithoutStackTrace  "Cannot reduce an empty node chain"
                    xs ->
                        let groups = M.toList $ groupBy head xs
                        in  forM_ groups $ forEachGroup rootName
            in reduceEach branches

        forEachGroup :: String -> (ExpandedNode, [ExpandedNodes]) -> State ReducedGraph ()
        forEachGroup rootName (start, branches) = error ""
            -- addEps . unique $ branches


        addEps :: [ExpandedNodes] -> [ExpandedNodes]
        addEps = \case
            []    -> []
            []:xs -> [EpsE]:addEps xs
            x:xs  -> x:addEps xs

        mergeCases = error ""
        mergeCase EpsE elts = error ""


