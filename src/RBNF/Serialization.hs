{-# LANGUAGE OverloadedStrings #-}
module RBNF.Serialization where
import           RBNF.Semantics
import           RBNF.Symbols
import           RBNF.Graph
import           RBNF.LookAHead
import           RBNF.IRs.Marisa
import           RBNF.Name
import           Data.Aeson

import qualified Data.Text.Lazy.Encoding       as T
import qualified Data.Text.Lazy.IO             as T



str_ :: String -> String
str_ = id
instance ToJSON VName where
  --
    toJSON = \case
        Slot  i -> object ["ctor" .= str_ "Slot", "value" .= i]
        Local s -> object ["ctor" .= str_ "Local", "value" .= s]

instance ToJSON IR where
    --     = IRAss VName IR
    --     | IRTuple [IR]
    --     | IRVar VName
    --     | IRMkSExp String IR
    --     | IRCall IR [IR]
    --     | IRPushScope
    --     | IRPopScope
    --     deriving (Eq, Ord)
    --
    toJSON a = case a of
        IRAss n ir ->
            object ["ctor" .= str_ "IRAss", "target" .= n, "value" .= ir]
        IRTuple xs -> object ["ctor" .= str_ "IRTuple", "elts" .= xs]
        IRVar   n  -> object ["ctor" .= str_ "IRVar", "name" .= n]
        IRMkSExp s ir ->
            object ["ctor" .= str_ "IRMkSExp", "name" .= s, "val" .= ir]
        IRCall f args ->
            object ["ctor" .= str_ "IRCall", "f" .= f, "args" .= args]

instance ToJSON Entity where
    -- | ENonTerm String
    -- | EPredicate IR
    -- | EModify IR
    --
    toJSON = \case
        ETerm      s  -> object ["ctor" .= str_ "ETerm", "val" .= s]
        ENonTerm   s  -> object ["ctor" .= str_ "ENonTerm", "name" .= s]
        EModify    ir -> object ["ctor" .= str_ "EModify", "code" .= ir]
        EPredicate ir -> object ["ctor" .= str_ "EPredicate", "code" .= ir]
        EBind name ir ->
            object ["ctor" .= str_ "EBind", "bind" .= name, "value" .= ir]
        EProc      irs -> object ["ctor" .= str_ "EProc", "codes" .= irs]
        EPopScope  s   -> object ["ctor" .= str_ "EPopScope", "scope" .= s]
        EPushScope s   -> object ["ctor" .= str_ "EPushScope", "scope" .= s]

instance ToJSON NodeKind where
    -- NEntity Entity
    -- | NProc [(Int, IR)] SlotIdx
    -- | DoNothing
    toJSON = \case
        NEntity e -> object ["ctor" .= str_ "NEntity", "val" .= e]
        NReturn i -> object ["ctor" .= str_ "NReturn", "slot" .= i]
        a         -> object ["ctor" .= show a]
    --
instance ToJSON Node where
  --
    toJSON Node { kind, _followed } =
        object ["kind" .= kind, "followed" .= _followed]


instance ToJSON Graph where
  --
    toJSON Graph { _nodes, _starts, _ends } =
        object ["nodes" .= _nodes, "starts" .= _starts, "ends" .= _ends]

instance FromJSON MiniLang
instance ToJSON MiniLang

instance FromJSON C
instance ToJSON C

instance FromJSON CGrammar
instance ToJSON CGrammar

instance FromJSON MName
instance ToJSON MName

instance FromJSON Marisa
instance ToJSON Marisa

dumpCG :: CGrammar -> _
dumpCG = T.decodeUtf8 . encode
readCG :: _ -> Maybe CGrammar
readCG = decode . T.encodeUtf8

dumpAIR :: Marisa -> _
dumpAIR = T.decodeUtf8 . encode
readAIR :: _ -> Maybe Marisa
readAIR = decode . T.encodeUtf8
