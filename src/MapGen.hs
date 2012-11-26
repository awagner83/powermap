module Main where

import Data.Conduit (runResourceT)
import Network.Wikipedia
import Network.HTTP.Conduit (newManager, def, closeManager)

main :: IO ()
main = do
    man <- newManager def
    resp2 <- runResourceT $ getLinks "Functional programming" man
    print resp2
    closeManager man

