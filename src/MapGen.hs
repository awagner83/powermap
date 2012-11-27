module Main where

import Network.Wikipedia

main :: IO ()
main = getNetwork "Category theory" 1 >>= print

