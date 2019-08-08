module RBNF.IRs.IRTrans where
import RBNF.IRs.Marisa
import RBNF.IRs.Reimu
import RBNF.IRs.TransMarisaToReimu


import qualified RBNF.IRs.ReimuTyping as RTyping
import RSolve.MultiState

import Control.Monad.State


import qualified Data.Set as S

class IRTransform a b c where
    irTransform :: a -> b
    irTransformWith :: c -> a -> b
    irTransformWith = error "not implemented"


instance IRTransform Marisa (Reimu ()) c where
    irTransform mk = flip evalState S.empty $ resolveDecl $
        evalState (marisaToReimu (return ()) mk) ()

instance IRTransform Marisa (Reimu RTyping.RT)  where
    irTransform =
        let env = emptyTCEnv emptyTInfo
        let res = flip runMS env $ do
            basicTCEnv True
            bWithAnnotated <- tc bWithDecl
            constrs <- getsMS $ view (ext . constr)
            let dnfs = unionEquations $ forM_ constrs assert
                ms =  flip L.map dnfs $ \dnf ->
                            forM dnf solve
                alts = case ms of
                        [] -> error "emm"
                        x:xs -> L.foldl (<|>) x xs
            alts
            bTyped <- pruneTypedBIR bWithAnnotated
            return $ typedBIRToDoc bTyped
    
    irTransform mk = flip evalState S.empty $ resolveDecl $
        evalState (marisaToReimu (return ()) mk) ()
