{-# LANGUAGE OverloadedStrings #-}

module Network.Wikipedia (getWikipedia) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy (ByteString())
import Data.Wikipedia.Request
import Data.Wikipedia.Response
import Network.HTTP.Conduit (parseUrl, httpLbs, requestHeaders,
                             responseBody, Manager)
import Network.HTTP.Types.Header

-- | Advertised User-agent
userAgent = "PowerMap-generator <awagner83@gmail.com>"

-- | Execute request against wikipedia api
getWikipedia req man = do
    req' <- liftIO $ parseUrl $ requestURL req
    let req'' = req' { requestHeaders = [(hUserAgent, userAgent)] }
    fmap responseBody $ httpLbs req'' man
        

