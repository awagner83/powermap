module Main where

import Control.Applicative
import Network.Wikipedia

main = print <$> (getWikipedia $ mkRequest "Category_theory")

mkRequest x = wikiLinks <> titles x <> pllimit "10"

