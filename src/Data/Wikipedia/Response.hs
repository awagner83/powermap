{-|
 - Wikipedia Links API response type
 -}
module Data.Wikipedia.Response where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Text (Text)

data Response = FinalResponse LinkAssoc
              | PartialResponse LinkAssoc ContToken

type LinkAssoc = HashMap Text [Text]
type ContToken = Text   -- plcontinue value

