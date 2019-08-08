module RBNF.BackEnds.TargetGen where

import RBNF.BackEnds.Merlin
import RBNF.IRs.Reimu
import RBNF.TypeSystem
import Data.Text.Prettyprint.Doc

class BackEnd a

data OCamlBackEnd = OCamlBackEnd
instance BackEnd OCamlBackEnd

class BackEnd end => TargetGen ir end where
    emit :: ir -> Doc end

instance TargetGen (Reimu RT) OCamlBackEnd where
    emit = ocamlGen