{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Wikipedia.Response.JSON (fromByteString, toProperResponse) where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy.Char8 hiding (map, zip, repeat)
import Data.Text hiding (map, zip)
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H
import qualified Data.Wikipedia.Response as R
import GHC.Generics

-- Data types

data Response = Response
    { queryContinue :: Maybe QueryCont , query :: Query }
    deriving (Generic, Show)

data QueryCont = QueryCont
    { links :: QueryContLinks } deriving (Generic, Show)

data QueryContLinks = QueryContLinks
    { plcontinue :: Text } deriving (Generic, Show)

data Query = Query
    { pages :: H.HashMap Text Page } deriving (Generic, Show)

data Page = Page
    { title :: Text, pageLinks :: [Text] } deriving (Generic, Show)


-- Instances

instance FromJSON Response where
    parseJSON (Object o) =
        Response <$> o .:? "query-continue" <*> o .: "query"

instance FromJSON Page where
    parseJSON (Object o) = do
        links <- mapM (.: "title") <$> o .: "links"
        Page <$> o .: "title" <*> links

instance FromJSON QueryCont
instance FromJSON QueryContLinks
instance FromJSON Query


-- | Decode bytestring json to intermediate json structure
fromByteString :: ByteString -> Maybe Response
fromByteString = decode

-- | Convert JSON Response (intermediate) to a proper Wikipedia Response
toProperResponse :: Response -> R.Response
toProperResponse x
    | Just qc <- queryContinue x = R.PartialResponse go (plcontinue $ links qc)
    | otherwise                  = R.FinalResponse go
    where go = M.fromList ps
          ps = map assoc $ H.elems $ pages (query x)
          assoc p = (title p, pageLinks p)

