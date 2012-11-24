{-# LANGUAGE OverloadedStrings #-}
{-|
 - Wikipedia Links API response type
 -}
module Data.Wikipedia.Response where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Map
import qualified Data.HashMap.Lazy as H
import Data.Text (Text)

data Response = FinalResponse LinkAssoc
              | PartialResponse LinkAssoc ContToken
              deriving Show

type LinkAssoc = Map Text (InE, OutE)
type ContToken = Text   -- plcontinue value
type InE       = [Text]
type OutE      = [Text]


-- | Build response value from Text
fromByteString :: ByteString -> Maybe Response
fromByteString = undefined

