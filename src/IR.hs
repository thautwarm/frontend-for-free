{-|
Description: intermediate representations of parsing rules
Author: thautwarm(Wanghongxuan Zhao) twshere@outlook.com
License: BSD-3-clause
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RBNF.IR where
import qualified Data.Map as M

data Token
    = Regex {name :: String, patt :: String}
    | StrsL {name :: String, vals :: [String]}
    deriving (Eq, Ord, Show)

-- an external function, producing
-- an INTEGER from next token or current state.
data Fun = Name String | Addr Int
    deriving (Eq, Ord, Show)

data Parser where

    -- P = f(P, State)
    RewP     :: Fun -> Parser
    -- P = if NAME=INT then P
    CaseP    :: Fun -> Int -> Parser
    -- P = P | P
    OrP      :: Parser -> Parser -> Parser
    -- P = P P
    AndP     :: Parser -> Parser -> Parser
    -- P = L
    LitP     :: Token  -> Parser
    -- P = P+
    RepP     :: Parser  -> Parser
    -- P = <String>
    RefP     :: String -> Parser
    deriving (Eq, Ord, Show)


type ENodes = ENode
-- Expanded node
data ENode where
    EpsE     :: ENode
    RewE     :: Fun -> ENode
    CaseE    :: Fun -> Int -> ENode
    RepE     :: [ENode] -> ENode
    RefE     :: String -> ENode
    LitE     :: Token  -> ENode
    PackE    :: String -> Int -> ENode
    deriving (Eq, Ord, Show)

stackEff :: ENode -> Int
stackEff = sum . fmap each
            where each :: ENode -> Int
                  each = \case
                    CaseE     -> 0
                    EpsE      -> 0
                    PackE _ n -> 1 - n
                    _         -> 1

type RNodes addr info = [RNode addr info]
-- Reduced node
data RNode addr info
    = Epsilon
    --  next: next set
    | RNode {nextSet :: RNodes addr info, info :: info}

-- Parsing graph
data PGraph addr info
    = PGraph { store    :: M.Map addr (RNode  addr info)
             , firstSet :: M.Map addr (RNodes addr info)}