{-# LANGUAGE OverloadedStrings, KindSignatures, FlexibleContexts,
             RankNTypes #-}

module Network.Wikipedia (getWikipedia, getLinks, getManyLinks) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.ByteString.Char8
import qualified Data.Text as T
import Data.Wikipedia.Request
import Data.Wikipedia.Response
import qualified Data.Wikipedia.Response.JSON as RJ
import Network.HTTP.Conduit (parseUrl, httpLbs, requestHeaders,
                             responseBody, Manager)
import Network.HTTP.Types.Header

import Debug.Trace

-- | Advertised User-agent
userAgent :: ByteString
userAgent = "PowerMap-generator <awagner83@gmail.com>"

-- | Get links for many pages at one time (may be several service calls)
getManyLinks
    :: forall (m :: * -> *). (MonadBaseControl IO m, MonadResource m)
    => Manager -> [String] -> m Response
getManyLinks man names = unions <$> mapM (getLinks man) names

-- | Get links from given wikipedia page (will exception on bad response)
getLinks
    :: forall (m :: * -> *). (MonadBaseControl IO m, MonadResource m)
    => Manager -> String -> m Response
getLinks man name = go baseReq
    where go req = do
            Just result <- traceShow (requestURL req) (getWikipedia req man)
            case result of
                a@(Final _)     -> return a
                a@(Partial _ c) -> return . union a =<< go (contReq c)
          baseReq = (wikiLinks <> titles name <> pllimit "500")
          contReq c = baseReq <> plcontinue (T.unpack c)

-- | Execute request against wikipedia api
getWikipedia
    :: forall (m :: * -> *). (MonadBaseControl IO m, MonadResource m)
    => Request -> Manager -> m (Maybe Response)
getWikipedia req man = do
    req' <- liftIO $ parseUrl $ requestURL req
    let req'' = req' { requestHeaders = [(hUserAgent, userAgent)] }
    t <- responseBody <$> httpLbs req'' man
    return (RJ.toProperResponse <$> RJ.fromByteString t)

