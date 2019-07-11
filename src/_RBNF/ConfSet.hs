{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module RBNF.ConfSet where

import RBNF.Grammar
import Control.Monad.State

-- Parsing item
class (Eq a, Ord a, Show a) => IsPItem a where
    startFrom  :: forall g. PGrammar g => PProd -> State g a
    prodIdx    :: forall g. PGrammar g => a -> State g Int
    prod       :: forall g. PGrammar g => a -> State g PProd
    dotPos     :: forall g. PGrammar g => a -> State g Int
    nextP      :: forall g. PGrammar g => a -> State g (Maybe P)
    next       :: forall g. PGrammar g => a -> State g (Maybe a)
    -- lookahead(1)
    closureLA1 :: forall g. PGrammar g => (a, Case) -> State g [(a, Case)]

class IsPItem a => IsConfSet m a where
    which   :: a -> m a -> Int
    closure ::  forall g. PGrammar g => m a -> State g (m a)
