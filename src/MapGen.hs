module Main where

import Data.Conduit (runResourceT)
import Network.Wikipedia
import Network.HTTP.Conduit (newManager, def, closeManager)

main :: IO ()
main = do
    man <- newManager def
    resp2 <- runResourceT $
        getManyLinks man ["Functional programming", "Academia"]
    print resp2
    closeManager man

