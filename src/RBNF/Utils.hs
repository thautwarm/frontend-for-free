module RBNF.Utils
(Set, Map, Lens', indent, groupBy, over, view, makeLenses, intercalate)
where
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import Control.Arrow
import Control.Lens (over, view, makeLenses, Lens')

type Set = S.Set
type Map = M.Map

indent n s = replicate n ' ' ++ s
groupBy f = M.fromListWith (++) . map (f &&& pure)
intercalate = L.intercalate