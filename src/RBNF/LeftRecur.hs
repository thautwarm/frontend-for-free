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
data SplitByIsLeftRecursive
    = SplitByIsLeftRecursive {_isLeftR :: [PRule], _notLeftR:: [PRule]}
    deriving (Show, Eq, Ord)

makeLenses ''SplitByIsLeftRecursive

mergeSplited :: [SplitByIsLeftRecursive] -> SplitByIsLeftRecursive
mergeSplited xs = SplitByIsLeftRecursive isLeftR' notLeftR'
  where
    extract f = concatMap (view f) xs
    isLeftR'  = extract isLeftR
    notLeftR' = extract notLeftR


markedLeftRecur :: PGrammarBuilder -> Grammar [P]
markedLeftRecur g =
    uncurry Grammar
        . (M.fromList . fst &&& M.fromList . snd)
        . unzip
        . M.elems
        . M.mapWithKey _SPILR2Pair
        $ fold pairs
  where
    _SPILR2Pair
        :: String
        -> SplitByIsLeftRecursive
        -> ((String, [PRule]), (String, [PRule]))
    _SPILR2Pair sym spilr =
        let f g = (sym, view g spilr) in (f notLeftR, f isLeftR)

    groups :: Map String [PRule]
    groups = M.map (map snd) $ groupBy fst g

    pairs  = M.toList groups

    detectLRNames leftRNamesAcc = \case
        [] -> leftRNamesAcc
        (sym, rules) : xs ->
            let recurs = S.insert sym leftRNamesAcc
                split  = mergeSplited $ map (splitLR sym recurs) rules
                leftRNamesAcc' | L.null (view isLeftR split) = leftRNamesAcc
                               | -- left recur symbol
                                 otherwise = recurs
            in detectLRNames leftRNamesAcc' xs

    lRNames = (\x -> trace (show x) x) $ detectLRNames S.empty pairs

    fold :: [(String, [PRule])] -> Map String SplitByIsLeftRecursive
    fold = \case
        [] -> M.empty
        (sym, rules) : xs ->
            let split  = mergeSplited $ map (splitLR sym lRNames) rules
            in  M.insert sym split $ fold xs

    splitLR :: String -> Set String -> PRule -> SplitByIsLeftRecursive
    splitLR root = frec
      where
        frec :: Set String -> PRule -> SplitByIsLeftRecursive
        frec recurs = \case
            []                 -> error "..." -- TODO: invalid prule
            rule@(PTerm _ : _) -> SplitByIsLeftRecursive [] [rule]
            rule@(PNonTerm name : xs)
                | S.member name recurs
                -> if name == root
                    then SplitByIsLeftRecursive [rule] []
                    else SplitByIsLeftRecursive [] [rule]
                | otherwise
                -> let recurs' = S.insert name recurs
                       arr     = case name `M.lookup` groups  of
                            Just arr -> arr
                            _        -> error $ "unknown terminal symbol " ++ name
                   in  mergeSplited $ map (frec recurs' . (++ xs)) arr
            x : xs ->
                let separated = frec recurs xs
                    addHd rules = [ x : rule | rule <- rules ]
                in  over isLeftR addHd . over notLeftR addHd $ separated
