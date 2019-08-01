{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module RBNF.LeftRecur where

import RBNF.Symbols
import RBNF.Grammar
import Control.Monad.State
import Control.Lens (makeLenses, Lens', over, view)
import Control.Arrow

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L

-- SPILR
data SplitByIsLeftRecursive
    = SplitByIsLeftRecursive {_isLeftR :: [PRule], _notLeftR:: [PRule]}
    deriving (Show, Eq, Ord)

makeLenses ''SplitByIsLeftRecursive

mergeSplited :: [SplitByIsLeftRecursive] -> SplitByIsLeftRecursive
mergeSplited xs = SplitByIsLeftRecursive isLeftR' notLeftR'
                  where
                    extract f = concatMap  (view f) xs
                    isLeftR'  = extract isLeftR
                    notLeftR' = extract notLeftR


markedLeftRecur :: PGrammarBuilder -> Grammar [P]
markedLeftRecur g =
        uncurry Grammar .
        (M.fromList . fst &&& M.fromList . snd) .
        unzip .
        M.elems .
        M.mapWithKey _SPILR2Pair .
        (M.mapWithKey f) $ groups
    where
        _SPILR2Pair :: String -> SplitByIsLeftRecursive -> ((String, [PRule]), (String, [PRule]))
        _SPILR2Pair sym spilr =
            let f g = (sym, view g spilr)
            in (f notLeftR, f isLeftR)

        groups :: Map String [PRule]
        groups = M.map (map snd) $ groupBy fst g

        f :: String -> [PRule] -> SplitByIsLeftRecursive
        f sym rules =
            let recurs = S.singleton sym
            in  mergeSplited $ map (splitLR sym recurs) rules
        splitLR :: String -> Set String -> PRule -> SplitByIsLeftRecursive
        splitLR root = frec
            where
                frec :: Set String -> PRule -> SplitByIsLeftRecursive
                frec recurs = \case
                    [] -> error "..." -- TODO: invalid prule
                    rule@(PTerm _:xs) -> SplitByIsLeftRecursive [] [rule]
                    rule@(PNonTerm name:xs)
                        | S.member name recurs ->
                            if name == root then SplitByIsLeftRecursive [rule] []
                            else SplitByIsLeftRecursive [] [rule]
                        | otherwise ->
                            let recurs' = S.insert name recurs
                                arr = groups M.! name
                            in  mergeSplited $
                                map (frec recurs' . (++ xs)) arr
                    x:xs ->
                        let separated = frec recurs xs
                            addHd rules = [x:rule | rule <- rules]
                        in over isLeftR addHd . over notLeftR addHd $ separated

