{-# LANGUAGE OverloadedStrings #-}

module Data.Wikipedia.Request
    ( Request ()    
    , (<>)
    , requestURL

    -- | Request properties
    , format
    , action
    , prop
    , titles
    , plnamespace
    , pllimit
    , plcontinue
    
    -- | Requests
    , queryRequest
    , linksRequest
    , wikiLinks
    ) where

import Data.List (intersperse)

data Request = Request [(String, String)] deriving Show

-- | Construct single-argument request
simpleRequest :: (String, String) -> Request
simpleRequest = Request . (:[])

-- | Join two requests into one
(<>) :: Request -> Request -> Request
(Request xs) <> (Request ys) = Request (xs ++ ys)

-- | Request properties
format, action, prop, titles, plnamespace, pllimit,
    plcontinue :: String -> Request
format      x = simpleRequest ("format", x)
action      x = simpleRequest ("action", x)
prop        x = simpleRequest ("prop", x)
titles      x = simpleRequest ("titles", x)
plnamespace x = simpleRequest ("plnamespace", x)
pllimit     x = simpleRequest ("pllimit", x)
plcontinue  x = simpleRequest ("plcontinue", x)

-- | Premade requests
queryRequest, linksRequest, wikiLinks :: Request
queryRequest = action "query"
linksRequest = queryRequest <> prop "links"
wikiLinks = linksRequest <> plnamespace "0"

-- | Base-url for wikipedia api
baseUrl :: String
baseUrl = "http://en.wikipedia.org/w/api.php"

-- | URL used to make request
requestURL :: Request -> String
requestURL (Request xs) = let query = concat $ intersperse "&" qargs
                              qargs = map (\(a, b) -> a ++ "=" ++ b) xs
                          in baseUrl ++ "?" ++ query

