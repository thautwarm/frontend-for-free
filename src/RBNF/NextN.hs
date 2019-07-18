{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module RBNF.NextN where

import RBNF.Symbols
import RBNF.Grammar
import RBNF.Follow

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Maybe (maybeToList)
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State

import Control.Lens (makeLenses, Lens', view, over)

-- shift-able symbols
data S
    = STerm Case
    | SNonTerm String
    deriving (Eq, Ord, Show)


data ConfSet = ConfSet { name :: String, remaining :: [S] }
    deriving (Eq, Ord, Show)


pToS = \case
    PNonTerm s -> Just (SNonTerm s)
    PTerm c    -> Just (STerm c)
    _          -> Nothing

pListToSList :: [P] -> [S]
pListToSList = \case
    [] -> []
    (pToS -> m):xs -> maybeToList m ++ pListToSList xs

pGrammarToSGrammar :: Grammar P -> Grammar S
pGrammarToSGrammar g =
    let prods' = view prods g
        leftR' = view leftR g
        sProds' = Map.map (map pListToSList) prods'
        sLeftR' = Map.map (map pListToSList) leftR'
    in Grammar sProds' sLeftR'


current :: ConfSet -> Maybe (S, ConfSet)
current confset =
  let rmn' = remaining confset
  in case rmn' of
      []   -> Nothing
      x:xs ->
        -- x must be PTerm or PNonTerm
        let new_conf = confset {remaining = xs}
        in Just (x, new_conf)

data LA
    = LASwitch [(Case, [LA])]
    | LAEnd Int
    | LANonTerm [Int] Case

-- a : {}
data NextNState = NextNState {
          _arg   :: ConfSet
        , _trace :: Set ConfSet
    }
makeLenses ''NextNState

-- expandFirst :: Grammar S -> Set String -> [S] -> [S]
-- expandFirst sg recurs = \case
--     xs@(STerm c:_) -> xs
--     x@(SNonTerm s: _) -> 

-- nextNImpl :: Grammar S -> ConfSet -> NextN
-- nextNImpl sg cs =
--     case cs of
--         [] ->
--             if n `Set.member` lr
--             then mergeNextN $ map (nextNImpl sg) (Map.elems lr)
--             else Suspend []
--     where
--         rmn = remaining cs
--         n   = name cs
--         lr  = view leftR sg

-- nextNImpl :: StateT NextNState (Reader (Grammar S)) NextN
-- nextNImpl = do
--     arg'   <- gets $ view arg
--     trace' <- gets $ view trace
--     if arg' `Set.member` trace'
--     then return $ Suspend arg'
--     else case remaining arg' of
--             [] -> do
--                 sg <- lift ask
--                 if name arg' `Map.member` view leftR sg
--                 then do

--                 modify $ over arg id
