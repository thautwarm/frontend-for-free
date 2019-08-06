module RBNF.Utils
(Set, Map, indent, groupBy, over, view, makeLenses, Lens')
where
import qualified Data.Map   as M
import qualified Data.Set   as S
import Control.Arrow
import Control.Lens (over, view, makeLenses, Lens')

type Set = S.Set
type Map = M.Map

indent n s = replicate n ' ' ++ s
groupBy f = M.fromListWith (++) . map (f &&& pure)