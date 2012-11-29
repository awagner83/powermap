{-# LANGUAGE OverloadedStrings #-}
{-|
 - Wikipedia Links API response type
 -}
module Data.Wikipedia.Response where

import Data.List (foldl1')
import qualified Data.Powermap as P
import Data.Text (Text, unpack)

data Response = Final P.Powermap
              | Partial P.Powermap ContToken
              deriving Show

type ContToken = Text   -- plcontinue value

-- | Combine two responses into one.  Result is always a Final
union :: Response -> Response -> Response
union (Final a) (Final b)         = Final $ P.union a b
union (Partial a _) b@(Final _)   = union (Final a)        b
union a@(Final _) (Partial b _)   = union        a  (Final b)
union (Partial a _) (Partial b _) = union (Final a) (Final b)

-- | Combine many responses into one.  Result is always a Final
unions :: [Response] -> Response
unions = foldl1' union

-- | The null response
empty :: Response
empty = Final P.empty

-- | List of all outgoing edges from given response
nextPages :: Response -> [String]
nextPages (Final xs)     = map unpack $ P.allOutE xs
nextPages (Partial xs _) = nextPages (Final xs)

-- | Extract powermap portion of response
toPowermap :: Response -> P.Powermap
toPowermap (Final x)     = x
toPowermap (Partial x _) = x

