module Hasura.Cache
  ( module Hasura.Cache.Types
  , B.CacheSize(..)
  , B.parseCacheSize
  , initialise
  ) where

import           Hasura.Prelude         hiding (lookup)

import           Control.Concurrent (getNumCapabilities)

import           Hasura.Cache.Types
import qualified Hasura.Cache.Bounded   as B
import qualified Hasura.Cache.Unbounded as U

initialise :: (Hashable k, Ord k) => Maybe B.CacheSize -> IO (Cache k v)
initialise cacheSizeM = do
  stripes <- getNumCapabilities
  case cacheSizeM of
    Nothing        -> Cache <$> U.initialise
    Just cacheSize -> Cache <$> B.initialise stripes cacheSize
