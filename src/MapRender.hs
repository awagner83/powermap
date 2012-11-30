{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Data.Binary
import Data.Powermap
import Data.Powermap.Util
import ReadArgs

main :: IO ()
main = do
    (file :: String, action :: String) <- readArgs
    p <- (decode <$> B.readFile file :: IO Powermap)

    putStrLn $ case action of
        "scores" -> unlines $ map show $ sortedVertextScores p
        "pretty" -> pretty p
        _        -> "usage: maprender POWERMAP_FILE (scores|pretty)"

