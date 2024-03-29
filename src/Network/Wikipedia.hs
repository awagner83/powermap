{-# LANGUAGE OverloadedStrings, KindSignatures, FlexibleContexts,
             RankNTypes #-}

module Network.Wikipedia
    ( getWikipedia
    , getLinks
    , getManyLinks
    , getNetwork) where

import Control.Applicative hiding (empty)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 hiding (empty, map, intercalate, splitAt)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Wikipedia.Request
import Data.Wikipedia.Response
import qualified Data.Wikipedia.Response.JSON as RJ
import Network.HTTP.Conduit (parseUrl, httpLbs, requestHeaders,
                             responseBody, Manager, withManager)
import Network.HTTP.Types.Header

import Debug.Trace

-- | Advertised User-agent
userAgent :: ByteString
userAgent = "powermap:mapgen (https://github.com/awagner83/powermap)"


-- | Get network of links starting from given page and moving N levels out
getNetwork :: String -> Integer -> IO Response
getNetwork page m = withManager (go [page] empty 0) where
    go ps resp n man
        | n > m     = return resp
        | otherwise = do
            resp' <- getManyLinks man ps
            go (nextPages resp') (union resp resp') (succ n) man

-- | Get links for many pages at one time (may be several service calls)
getManyLinks
    :: forall (m :: * -> *). (MonadBaseControl IO m, MonadResource m)
    => Manager -> [String] -> m Response
getManyLinks man names = unions <$> mapM (getLinks man) nameGroups
    where nameGroups = groupAndJoin "|" names 25

-- | Get links from given wikipedia page (will exception on bad response)
getLinks
    :: forall (m :: * -> *). (MonadBaseControl IO m, MonadResource m)
    => Manager -> String -> m Response
getLinks man name = trace name (go baseReq)
    where go req = do
            Just result <- getWikipedia req man
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

-- | Break list into N sized chunks
groupsOf :: [a] -> Int -> [[a]]
groupsOf [] _ = []
groupsOf xs n = let (h, t) = splitAt n xs in (h : groupsOf t n)

-- | Join chunks of list with given delimiter
groupAndJoin :: [a] -> [[a]] -> Int -> [[a]]
groupAndJoin d xs = map (intercalate d) . groupsOf xs

