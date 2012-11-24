{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy.Char8
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H
import GHC.Generics

-- Data types

data Request = Request
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

instance FromJSON Request where
    parseJSON (Object o) =
        Request <$> o .:? "query-continue" <*> o .: "query"

instance FromJSON Page where
    parseJSON (Object o) = do
        links <- mapM (\l -> l .: "title") <$> o .: "links"
        Page <$> o .: "title" <*> links

instance FromJSON QueryCont
instance FromJSON QueryContLinks
instance FromJSON Query

