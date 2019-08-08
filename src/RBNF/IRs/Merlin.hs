-- | Merlin stands for the OCaml back end
module RBNF.IRs.Merlin where
import RBNF.IRs.Reimu
import RBNF.TypeSystem
import GHC.Generics
-- | The spelling of Meirin is similar to Merlin.
--   Merlin is OCaml Programming Language's leading IDE plugin.
data Merlin
    = MLetIn 
-- ocamlGen :: Reimu RT -> 