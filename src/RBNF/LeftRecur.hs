-- | Solving left recursions and producing parsing grammar structs(RBNF.Grammar.Grammar)
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-07-13
-- License: BSD-3-clause
-- {-# OPTIONS_GHC -Wall #-}
module RBNF.LeftRecur where

import           RBNF.Utils
import           RBNF.Symbols
import           RBNF.Grammar

import           Control.Arrow
import           Control.Monad.State

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.List                     as L

-- hereafter as SPILR
data SplitByLR
    = SplitByLR {_isLeftR :: [PRule], _notLeftR:: [PRule]}
    deriving (Show, Eq, Ord)

makeLenses ''SplitByLR

mergeSplited :: [SplitByLR] -> SplitByLR
mergeSplited xs = SplitByLR isLeftR' notLeftR'
  where
    extract f = L.nub $ concatMap (view f) xs
    isLeftR'  = extract isLeftR
    notLeftR' = extract notLeftR


getProd :: Map String [PRule] -> String -> [PRule]
getProd g k = case M.lookup k g of
    Just v -> v
    _      -> error $ "unknown terminal symbol " ++ k

data LRStep
    = LRStep { syms :: Set String
             , itrs :: [(String, SplitByLR)]
             , lrs  :: [String]
             , news :: [String]
             }

markedLeftRecur :: String -> PGrammarBuilder -> Grammar [P]
markedLeftRecur top g =
    uncurry Grammar
        . (M.fromList . fst &&& M.fromList . snd)
        . unzip
        . M.elems
        . M.mapWithKey splitToPair
        . M.fromList
        $ itrs final
    where
        splitToPair :: String -> SplitByLR -> ((String, [PRule]), (String, [PRule]))
        splitToPair sym spilr = let f g = (sym, view g spilr) in (f notLeftR, f isLeftR)
        groups :: Map String [PRule]
        groups = M.map (map snd) $ groupBy fst g

        init   = LRStep {syms=S.empty, itrs=[], lrs=[], news=[top]}
        final  = iteratorFunc init
        -- fix point
        iteratorFunc :: LRStep -> LRStep
        iteratorFunc a@LRStep {news=[]} = a
        iteratorFunc LRStep {syms, itrs, lrs, news} =
            iteratorFunc $ LRStep {syms=S.union syms symsDiff, itrs=newItrs ++ itrs, lrs=lrs', news=S.toList symsDiff}
            where
                (lrs', newItrs) = fold lrs [(sym, getProd groups sym) | sym <- news]
                symRefs = S.unions [unionRefs $ (ps1 ++ ps2) | (_, SplitByLR ps1 ps2) <- newItrs]
                symsDiff = S.difference symRefs syms -- new news

        fold :: [String] -> [(String, [PRule])] -> ([String], [(String, SplitByLR)])
        fold lrs = \case
            [] -> (lrs, [])
            (sym, rules) : xs ->
                let split  = mergeSplited $ map (splitLR sym lrs) rules
                    lrs' | L.null $ view isLeftR split = lrs
                        | otherwise                   = sym:lrs
                    (lrs'', m) = fold lrs' xs
                in  (lrs'', (sym, split):m)

        unionRefs :: [PRule] -> Set String
        unionRefs = foldl (\a b -> S.union a (refs b)) S.empty
        refs :: PRule -> Set String
        refs = \case
            [] -> S.empty
            PTerm _:xs    -> refs xs
            PNonTerm n:xs -> S.insert n $ refs xs
            x:xs          -> refs xs

        splitLR :: String -> [String] -> PRule -> SplitByLR
        splitLR root lrs = frec $ S.singleton root
            where
                frec :: Set String -> PRule -> SplitByLR
                frec recurs = \case
                    []                 -> error "..." -- TODO: invalid prule
                    rule@(PTerm _ : _) -> SplitByLR [] [rule]
                    rule@(PNonTerm name : xs)
                        | S.member name recurs
                        -> if name == root
                            then SplitByLR [rule] []
                            else SplitByLR [] [rule]
                        | name `elem` lrs
                        -> SplitByLR [] [rule]
                        | otherwise
                        -> mayExpandFst where
                                recurs' = S.insert name recurs
                                arr     = case name `M.lookup` groups  of
                                    Just arr -> arr
                                    _        -> error $ "unknown terminal symbol " ++ name
                                split  = mergeSplited $ map (frec recurs' . (++ xs)) arr
                                mayExpandFst -- if not left recur, do not expand(then avoid reduce-reduce conflicts)!
                                    | L.null $ view isLeftR split = SplitByLR [] [rule]
                                    | otherwise = split
                    x : xs ->
                        let separated = frec recurs xs
                            addHd rules = [ x : rule | rule <- rules ]
                        in  over isLeftR addHd . over notLeftR addHd $ separated

-- markedLeftRecur :: PGrammarBuilder -> Grammar [P]
-- markedLeftRecur g =
--     uncurry Grammar
--         . (M.fromList . fst &&& M.fromList . snd)
--         . unzip
--         . M.elems
--         . M.mapWithKey _SPILR2Pair
--         $ fold [] pairs
--   where
--     _SPILR2Pair
--         :: String
--         -> SplitByLR
--         -> ((String, [PRule]), (String, [PRule]))
--     _SPILR2Pair sym spilr =
--         let f g = (sym, view g spilr) in (f notLeftR, f isLeftR)

--     groups :: Map String [PRule]
--     groups = M.map (map snd) $ groupBy fst g

--     pairs  = M.toList groups

--     fold :: [String] -> [(String, [PRule])] -> Map String SplitByLR
--     fold lrs = \case
--         [] -> M.empty
--         (sym, rules) : xs ->
--             let split  = mergeSplited $ map (splitLR sym lrs) rules
--                 lrs' | L.null $ view isLeftR split = lrs
--                      | otherwise                   = sym:lrs
--             in  M.insert sym split $ fold lrs' xs




    -- fold = fold' []
    -- fold' :: [String] -> [(String, [PRule])] -> Map String SplitByLR
    -- fold' processed = \case
    --     [] -> M.empty
    --     (sym, rules) : xs ->
    --         let split  = mergeSplited $ map (splitLR sym processed) rules
    --         in  M.insert sym split $ fold' (sym:processed) xs

    -- splitLR :: String -> [String] -> PRule -> SplitByLR
    -- splitLR root processed = frec S.empty
    --   where
    --     frec :: Set String -> PRule -> SplitByLR
    --     frec recurs = \case
    --         []                 -> error "..." -- TODO: invalid prule
    --         rule@(PTerm _ : _) -> SplitByLR [] [rule]
    --         rule@(PNonTerm name : xs)
    --             | S.member name recurs
    --             -> if name == root
    --                 then SplitByLR [rule] []
    --                 else SplitByLR [] [rule]
    --             | name `elem` processed
    --             -> SplitByLR [] [rule]
    --             | otherwise
    --             -> let recurs' = S.insert name recurs
    --                    arr     = case name `M.lookup` groups  of
    --                         Just arr -> arr
    --                         _        -> error $ "unknown terminal symbol " ++ name
    --                in  mergeSplited $ map (frec recurs' . (++ xs)) arr
    --         x : xs ->
    --             let separated = frec recurs xs
    --                 addHd rules = [ x : rule | rule <- rules ]
    --             in  over isLeftR addHd . over notLeftR addHd $ separated
