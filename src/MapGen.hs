{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStr)
import Network.Wikipedia
import Data.Binary
import Data.ByteString.Lazy (putStr)
import Data.Wikipedia.Response
import ReadArgs

main :: IO ()
main = do
    (topic :: String, nSteps :: Integer) <- readArgs
    getNetwork topic nSteps >>= (putStr . encode . toPowermap)

