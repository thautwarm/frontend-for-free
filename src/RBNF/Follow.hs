{-# LANGUAGE LambdaCase #-}

module RBNF.Follow where
import Control.Arrow
import RBNF.Symbols
import RBNF.Grammar
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

followTerms :: PGrammarBuilder -> String -> [P]
followTerms g which = followTermsImpl (S.singleton which) which g

followTermsImpl recurs which g = concat follows
    where
        follows = map calcFollow g
        calcFollow :: PProd -> [P]
        calcFollow (sym, x:xs) = calcFollowed sym x xs
        calcFollowed sym (PNonTerm s) = \case
            []
                | s `S.member` recurs -> []
                | otherwise -> followTermsImpl (S.insert s recurs) s g
            h@(PTerm _):xs ->
                h:calcFollowed sym h xs
            h@(PNonTerm _):xs ->
                h:calcFollowed sym h xs
            _:xs -> calcFollowed sym (PNonTerm s) xs
        calcFollowed sym _ = \case
            [] -> []
            x:xs -> calcFollowed sym x xs

followSet :: PGrammarBuilder -> Map String [P]
followSet g =
    let syms = L.nub $ map fst g
    in  M.fromList $ map (id &&& L.nub . followTerms g) syms