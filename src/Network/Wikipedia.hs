{-# LANGUAGE OverloadedStrings #-}

module Network.Wikipedia 
    ( Request ()    
    , (<>)
    , getWikipedia

    -- | Request properties
    , format
    , action
    , prop
    , titles
    , plnamespace
    , pllimit
    
    -- | Requests
    , queryRequest
    , linksRequest
    , wikiLinks
    ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy (ByteString())
import Data.List (intersperse)
import Network.HTTP.Conduit (parseUrl, httpLbs, requestHeaders,
                             responseBody, Manager)
import Network.HTTP.Types.Header

data Request = Request [(String, String)] deriving Show

-- | Construct single-argument request
simpleRequest :: (String, String) -> Request
simpleRequest = Request . (:[])

-- | Join two requests into one
(<>) :: Request -> Request -> Request
(Request xs) <> (Request ys) = Request (xs ++ ys)

-- | Request properties
format, action, prop, titles, plnamespace, pllimit :: String -> Request
format      x = simpleRequest ("format", x)
action      x = simpleRequest ("action", x)
prop        x = simpleRequest ("prop", x)
titles      x = simpleRequest ("titles", x)
plnamespace x = simpleRequest ("plnamespace", x)
pllimit     x = simpleRequest ("pllimit", x)

-- | Premade requests
queryRequest, linksRequest, wikiLinks :: Request
queryRequest = action "query"
linksRequest = queryRequest <> prop "links"
wikiLinks = linksRequest <> plnamespace "0"

-- | Base-url for wikipedia api
baseUrl :: String
baseUrl = "http://en.wikipedia.org/w/api.php"

-- | Advertised User-agent
userAgent = "PowerMap-generator <awagner83@gmail.com>"


-- | URL used to make request
requestURL :: Request -> String
requestURL (Request xs) = let query = concat $ intersperse "&" qargs
                              qargs = map (\(a, b) -> a ++ "=" ++ b) xs
                          in baseUrl ++ "?" ++ query

-- | Execute request against wikipedia api
--getWikipedia :: Request -> Manager -> IO ByteString
getWikipedia req man = do
    req' <- liftIO $ parseUrl $ requestURL req
    let req'' = req' { requestHeaders = [(hUserAgent, userAgent)] }
    fmap responseBody $ httpLbs req'' man
        

