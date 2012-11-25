{-# LANGUAGE OverloadedStrings, KindSignatures, FlexibleContexts,
             RankNTypes #-}

module Network.Wikipedia (getWikipedia) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.ByteString.Char8
import Data.Wikipedia.Request
import Data.Wikipedia.Response
import qualified Data.Wikipedia.Response.JSON as RJ
import Network.HTTP.Conduit (parseUrl, httpLbs, requestHeaders,
                             responseBody, Manager)
import Network.HTTP.Types.Header

-- | Advertised User-agent
userAgent :: ByteString
userAgent = "PowerMap-generator <awagner83@gmail.com>"

-- | Execute request against wikipedia api
getWikipedia :: forall (m :: * -> *).
             (MonadBaseControl IO m, MonadResource m)
             => Request -> Manager -> m (Maybe Response)
getWikipedia req man = do
    req' <- liftIO $ parseUrl $ requestURL req
    let req'' = req' { requestHeaders = [(hUserAgent, userAgent)] }
    t <- responseBody <$> httpLbs req'' man
    return (RJ.toProperResponse <$> RJ.fromByteString t)

