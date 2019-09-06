module RBNF.BackEnds.TargetGen where

import RBNF.BackEnds.Merlin
import RBNF.BackEnds.Pyrrha
import RBNF.IRs.Reimu
import RBNF.IRs.Marisa
import RBNF.TypeSystem
import Data.Text.Prettyprint.Doc

class BackEnd a

data OCamlBackEnd = OCamlBackEnd
instance BackEnd OCamlBackEnd

data PythonBackEnd = PythonBackEnd
instance BackEnd PythonBackEnd

class BackEnd end => TargetGen ir end where
    emit :: ir -> Doc end

instance TargetGen (Reimu RT) OCamlBackEnd where
    emit = ocamlGen

instance TargetGen Marisa PythonBackEnd where
    emit = pretty . pyGen