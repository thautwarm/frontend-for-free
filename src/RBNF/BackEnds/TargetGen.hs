module RBNF.BackEnds.TargetGen where
import RBNF.BackEnds.Python
import RBNF.BackEnds.Julia
import RBNF.IRs.IRTrans
import RBNF.IRs.Marisa
import Data.Text.Prettyprint.Doc

class BackEnd a

data PythonBackEnd = PythonBackEnd
data JuliaBackEnd = JuliaBackEnd

instance BackEnd PythonBackEnd
instance BackEnd JuliaBackEnd


class BackEnd end => TargetGen ir end where
    emit :: ir -> Doc end

instance TargetGen Marisa PythonBackEnd where
    emit = pretty . pyGen

instance TargetGen Marisa JuliaBackEnd where
    emit = genJl True