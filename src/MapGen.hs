module Main where

import Control.Applicative
import Data.Conduit (runResourceT)
import Network.Wikipedia
import Network.HTTP.Conduit (newManager, def, closeManager)

main = do
    man <- newManager def
    resp <- runResourceT $ getWikipedia (mkRequest "Category_theory") man
    print resp
    resp2 <- runResourceT $ getWikipedia (mkRequest "Algebra") man
    print resp
    closeManager man

mkRequest x = wikiLinks <> titles x <> pllimit "100" <> format "json"

