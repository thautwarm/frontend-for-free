module RBNF.BackEnds.TargetGen where
import RBNF.BackEnds.Pyrrha
import RBNF.IRs.IRTrans
import RBNF.IRs.Marisa
import Data.Text.Prettyprint.Doc

class BackEnd a

data PythonBackEnd = PythonBackEnd
instance BackEnd PythonBackEnd

class BackEnd end => TargetGen ir end where
    emit :: ir -> Doc end

instance TargetGen Marisa PythonBackEnd where
    emit = pretty . pyGen