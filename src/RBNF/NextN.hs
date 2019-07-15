{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RBNF.NextN where

import RBNF.Symbols
import RBNF.Grammar
import RBNF.Follow

import Control.Arrow
import Control.Monad.State

data ConfSet = ConfSet {
      _lhs  :: String
    , _rhs  :: PRule
    , _pos  :: Int
    }

data NextN
    = NextN   [(Case, NextN)]
    | Suspend ConfSet
