{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Wikipedia
import ReadArgs

main :: IO ()
main = do
    (topic :: String, nSteps :: Integer) <- readArgs
    getNetwork topic nSteps >>= print

