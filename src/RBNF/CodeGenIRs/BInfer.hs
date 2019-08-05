-- | Type inference for BIR
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-06
-- License: BSD-3-clause
-- {-# LANGUAGE FlexibleContexts #-}

module RBNF.CodeGenIRs.BInfer where

import RBNF.CodeGenIRs.ABuiltins
import RSolve.PropLogic
import RSolve.MultiState
import RSolve.HM

import RBNF.Utils

-- All here are reference types!
data BTPrim
    = BTInt
    | BTFloat
    | BTUnit
    | BTString
    | BTAny

data BT
    = BTPrim BTPrim
    | Tuple [BT]
    | BTFunc BT BT
    | BTStruct [(String, BT)]

-- | Each module will hold a 'TInfo'
data TInfo
    = TInfo {
          types  :: Map Int BTPrim
          -- | a map that maps a field name to
          -- candicate lists of (structType, fieldType)
        , fields :: Map String [(T, T)]
       }

-- basicTCEnv :: []
-- basicTCEnv = do
--     i <- newTNom