module RBNF.Serialization where
import RBNF.Symbols
import Data.Aeson

instance FromJSON Case
instance ToJSON Case

instance FromJSON MiniLang
instance ToJSON MiniLang

instance FromJSON C
instance ToJSON C