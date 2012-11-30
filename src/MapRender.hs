{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Powermap
import ReadArgs

main :: IO ()
main = do
    (file :: String) <- readArgs
    putStrLn . pretty . decode =<< B.readFile file

