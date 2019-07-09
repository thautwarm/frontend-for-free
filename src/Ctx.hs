{-|
Description: Parsing analysis context
Author: thautwarm(Wanghongxuan Zhao) twshere@outlook.com
License: BSD-3-clause
-}

module RBNF.Ctx where
import RBNF.IR

data PCtx
    = PCtx {
        graph :: PGraph
      , 
    }