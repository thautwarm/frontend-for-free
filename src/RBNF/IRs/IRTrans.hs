module RBNF.IRs.IRTrans where
import RBNF.IRs.Marisa
import RBNF.IRs.Reimu
import RBNF.IRs.ReimuTyping (solveReimu)

import RBNF.IRs.TransMarisaToReimu
import RBNF.TypeSystem (RT)

import qualified RBNF.IRs.ReimuTyping as RTyping
import RSolve.MultiState

import Control.Monad.State


import qualified Data.Set as S

class IRTransform a b where
    irTransform :: a -> b


instance IRTransform Marisa (Reimu ()) where
    irTransform mk = flip evalState S.empty $ resolveDecl $
        evalState (marisaToReimu (return ()) mk) ()

instance IRTransform (Reimu ()) [Reimu RT]  where
    irTransform  = solveReimu

instance IRTransform Marisa [Reimu RT]  where
    irTransform  = solveReimu . middleTrans
        where middleTrans :: Marisa -> Reimu ()
              middleTrans = irTransform