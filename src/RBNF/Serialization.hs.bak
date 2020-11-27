{-# LANGUAGE OverloadedStrings #-}
module RBNF.Serialization where
import           RBNF.Semantics
import           RBNF.Constructs
import           RBNF.Graph
import           RBNF.LookAHead
import           RBNF.IRs.Marisa
import           RBNF.Name
import           RBNF.Utils
import           Data.Aeson

import qualified Data.Text.Lazy.Encoding       as T
import qualified Data.Text.Lazy.IO             as T



str_ :: String -> String
str_ = id

instance ToJSON Name
instance FromJSON Name


instance ToJSON S where
    toJSON a = case a of
        SAss n ir ->
            object ["ctor" .= str_ "IRAss", "target" .= n, "value" .= ir]
        STp xs -> object ["ctor" .= str_ "IRTuple", "elts" .= xs]
        SVar   n  -> object ["ctor" .= str_ "IRVar", "name" .= n]
        SExp s ir ->
            object ["ctor" .= str_ "IRMkSExp", "name" .= s, "val" .= ir]
        SCall f args ->
            object ["ctor" .= str_ "IRCall", "f" .= f, "args" .= args]

instance ToJSON Entity where
    toJSON = \case
        ETerm      s  -> object ["ctor" .= str_ "ETerm", "val" .= s]
        ENonTerm   s  -> object ["ctor" .= str_ "ENonTerm", "name" .= s]
        EProc      irs -> object ["ctor" .= str_ "EProc", "codes" .= irs]

instance ToJSON NodeKind where
    -- NEntity Entity
    -- | NProc [(Int, IR)] SlotIdx
    -- | DoNothing
    toJSON = \case
        NEntity e -> object ["ctor" .= str_ "NEntity", "val" .= e]
        NReturn i -> object ["ctor" .= str_ "NReturn", "slot" .= i]
        a         -> object ["ctor" .= show a]

instance ToJSON Node where
    toJSON Node { kind, _followed } =
        object ["kind" .= kind, "followed" .= _followed]


instance ToJSON Graph where
    toJSON Graph { _nodes, _starts, _ends } =
        object ["nodes" .= _nodes, "starts" .= _starts, "ends" .= _ends]

instance FromJSON MiniLang
instance ToJSON MiniLang

instance FromJSON C
instance ToJSON C

instance FromJSON Marisa
instance ToJSON Marisa

dumpAIR :: Marisa -> _
dumpAIR = T.decodeUtf8 . encode
readAIR :: _ -> Maybe Marisa
readAIR = decode . T.encodeUtf8
