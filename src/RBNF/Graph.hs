module RBNF.Graph where

import Control.Monad.State
import RBNF.Symbols
import RBNF.Grammar

data NextN
    = NextN (Map Case NextN)
    | AnyCase

followSet :: P -> State PGrammar [Case]
followSet = error ""
firstSet  :: P -> State PGrammar [Case]
firstSet = error ""