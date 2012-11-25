module Main where

import Data.Conduit (runResourceT)
import Data.Wikipedia.Request
import Network.Wikipedia
import Network.HTTP.Conduit (newManager, def, closeManager)

main :: IO ()
main = do
    man <- newManager def
    resp <- runResourceT $ getWikipedia' (mkRequest "Category_theory") man
    print resp
    resp2 <- runResourceT $ getWikipedia' (mkRequest "Algebra") man
    print resp2
    closeManager man

mkRequest :: String -> Request
mkRequest x = wikiLinks <> titles x <> pllimit "100" <> format "json"

