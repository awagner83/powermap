module Main where

import Data.Conduit (runResourceT)
import Data.Wikipedia.Request
import Network.Wikipedia
import Network.HTTP.Conduit (newManager, def, closeManager)

main :: IO ()
main = do
    man <- newManager def
    resp <- runResourceT $ getWikipedia' (mkRequest "Arithmetic") man
    print resp
    closeManager man

mkRequest :: String -> Request
mkRequest x = wikiLinks <> titles x <> pllimit "500" <> format "json"

