{-# LANGUAGE LambdaCase #-}
module RBNF.GraphAnalysis.Reduce where
import RBNF.Semantics
import RBNF.GraphAnalysis.IRs
import qualified Data.Map as M    
import qualified Data.Set as S    

groupNodes :: [ExpandedNodes] ->  M.Map ExpandedNode [ExpandedNodes]
groupNodes [] = M.empty
groupNodes (x :: xs) =
    let m = groupNodes xs
        (hd:tl) = x
        insert_f :: [ExpandedNodes] -> [ExpandedNodes] -> [ExpandedNodes]
        insert_f [one] old = one:old
    in M.insertWith insert_f hd [tl] m

groupNodes f xs = M.fromListWith (++) $ [(f e, e) | e <- xs]

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
            in unique' occurred' xs'        

reduce :: ExpandedGraph -> ReducedGraph
reduce ctx =
    fmap reduceEach ctx
    where
        reduceEach :: [ExpandedNodes] -> ReducedNode
        reduceEach = \case
            [] -> errorWithoutStackTrace  "Cannot reduce an empty node chain"
            xs -> mergeCases . M.toList . fmap (addEqs . unique) . groupNodes $ xs
        addEps :: [ExpandedNodes] -> [ExpandedNodes]
        addEps = \case
            []    -> []
            []:xs -> [EpsE]:addEps xs
            x:xs  -> x:addEps xs
        
        mergeCase EpsE elts =

            
            