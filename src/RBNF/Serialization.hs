{-# LANGUAGE PartialTypeSignatures #-}
module RBNF.Serialization where
import RBNF.Symbols
import RBNF.CodeGenIRs.A
import Data.Aeson

import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

instance FromJSON MiniLang
instance ToJSON MiniLang

instance FromJSON C
instance ToJSON C

instance FromJSON CGrammar
instance ToJSON CGrammar

instance FromJSON AName
instance ToJSON AName

instance FromJSON AIR
instance ToJSON AIR

dumpCG :: CGrammar -> _
dumpCG = T.decodeUtf8 . encode
readCG :: _ -> Maybe CGrammar
readCG = decode . T.encodeUtf8

dumpAIR :: AIR -> _
dumpAIR = T.decodeUtf8 . encode
readAIR :: _ -> Maybe AIR
readAIR = decode . T.encodeUtf8