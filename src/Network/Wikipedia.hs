{-# LANGUAGE OverloadedStrings #-}

module Network.Wikipedia (getWikipedia) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Wikipedia.Request
import Data.Wikipedia.Response
import Network.HTTP.Conduit (parseUrl, httpLbs, requestHeaders,
                             responseBody)
import Network.HTTP.Types.Header

-- | Advertised User-agent
userAgent = "PowerMap-generator <awagner83@gmail.com>"

-- | Execute request against wikipedia api
getWikipedia req man = do
    req' <- liftIO $ parseUrl $ requestURL req
    let req'' = req' { requestHeaders = [(hUserAgent, userAgent)] }
    fmap responseBody $ httpLbs req'' man


getWikipedia' req man = do
    t <- getWikipedia req man
    return (fromByteString t)
        

