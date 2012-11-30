{-# LANGUAGE TupleSections #-}
{- Grab-bag of utility functions for viewing powermap data -}
module Data.Powermap.Util where

import qualified Data.Map as M
import Control.Applicative
import Data.List
import Data.Ord
import Data.Powermap

type Score = Int

-- | Map of vertex scores
vertexScores :: Powermap -> M.Map Vertex Score
vertexScores = M.fromListWith (+) . scores . M.toList . getPowermap
    where scores = fmap (,1) . concatMap snd

-- | Sorted list of vertices by score
sortedVertextScores :: Powermap -> [(Vertex, Score)]
sortedVertextScores = reverse . sortBy (comparing snd) . M.toList . vertexScores

