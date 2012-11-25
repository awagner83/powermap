{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Wikipedia.Response.JSON (fromByteString) where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy.Char8
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H
import GHC.Generics

-- Data types

data Response = Response
    { queryContinue :: Maybe QueryCont , query :: Query }
    deriving (Generic, Show)

data QueryCont = QueryCont
    { links :: QueryContLinks } deriving (Generic, Show)

data QueryContLinks = QueryContLinks
    { plcontinue :: String } deriving (Generic, Show)

data Query = Query
    { pages :: H.HashMap String Page } deriving (Generic, Show)

data Page = Page
    { title :: String, pageLinks :: [String] } deriving (Generic, Show)


-- Instances

instance FromJSON Response where
    parseJSON (Object o) =
        Response <$> o .:? "query-continue" <*> o .: "query"

instance FromJSON Page where
    parseJSON (Object o) = do
        links <- mapM (\l -> l .: "title") <$> o .: "links"
        Page <$> o .: "title" <*> links

instance FromJSON QueryCont
instance FromJSON QueryContLinks
instance FromJSON Query

-- | Decode bytestring json to intermediate json structure
fromByteString :: ByteString -> Maybe Response
fromByteString = decode

