module Data.Powermap where

import qualified Data.Map as M
import Control.Applicative
import Data.Binary
import Data.Text (Text, pack, unpack)

newtype Powermap = Powermap { getPowermap :: M.Map Vertex OutE } deriving Show
type OutE   = [Vertex]
type Vertex = Text

instance Binary Powermap where
    put = put . getPowermap
    get = Powermap <$> get

instance Binary Text where
    put = put . unpack
    get = pack <$> get

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

