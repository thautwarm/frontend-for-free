module RBNF.IRs.IRTrans where
import RBNF.IRs.Marisa
import Control.Monad.State


import qualified Data.Set as S

class IRTransform a b where
    irTransform :: a -> b
