module RBNF.Utils
(
Set, Map, Lens', TaggedFixT(..),
indent, groupBy, over, view, makeLenses, intercalate,
mangling, pack, Text, (&&&)
)
where
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import Control.Arrow
import Control.Lens (over, view, makeLenses, Lens')
import Data.Text(pack, unpack, replace, Text, append)

type Set = S.Set
type Map = M.Map

indent n s = replicate n ' ' ++ s
groupBy f = M.fromListWith (++) . map (f &&& pure)
intercalate = L.intercalate

data TaggedFixT f t = InT {tag :: t, outT :: f (TaggedFixT f t)}
deriving instance Functor f => Functor (TaggedFixT f)
deriving instance Foldable f => Foldable (TaggedFixT f)
deriving instance Traversable f => Traversable (TaggedFixT f)


mangling :: Text -> Text -> Text -> Text
mangling a b = replace a b . replace b (append b b)