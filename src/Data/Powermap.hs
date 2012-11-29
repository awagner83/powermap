module Data.Powermap where

import qualified Data.Map as M
import Data.Text (Text)

newtype Powermap = Powermap { getPowermap :: M.Map Vertex OutE } deriving Show
type OutE   = [Vertex]
type Vertex = Text

-- | Map from new assoc list
fromList :: [(Vertex, OutE)] -> Powermap
fromList = Powermap . M.fromList

-- | Outbound edges for all vertices in map
allOutE :: Powermap -> OutE
allOutE = concat . M.elems . getPowermap

-- | The empty map
empty :: Powermap
empty = Powermap $ M.empty

-- | Combine two maps into one
union :: Powermap -> Powermap -> Powermap
union a b = Powermap $ M.unionWith (++) (getPowermap a) (getPowermap b)
