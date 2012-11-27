{-# LANGUAGE OverloadedStrings #-}
{-|
 - Wikipedia Links API response type
 -}
module Data.Wikipedia.Response where

import Data.List (foldl1')
import Data.Map (Map(), unionWith)
import Data.Text (Text)

data Response = Final LinkAssoc
              | Partial LinkAssoc ContToken
              deriving Show

type LinkAssoc = Map Text OutE
type ContToken = Text   -- plcontinue value
type OutE      = [Text]

-- | Combine two responses into one.  Result is always a Final
union :: Response -> Response -> Response
union (Final a) (Final b)         = Final (unionWith (++) a b)
union (Partial a _) b@(Final _)   = union (Final a)        b
union a@(Final _) (Partial b _)   = union        a  (Final b)
union (Partial a _) (Partial b _) = union (Final a) (Final b)

-- | Combine many responses into one.  Result is always a Final
unions :: [Response] -> Response
unions = foldl1' union

