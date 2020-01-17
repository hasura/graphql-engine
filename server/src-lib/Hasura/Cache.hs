module Hasura.Cache
  ( Cache
  , CacheOptions
  , mkCacheOptions
  , B.CacheSize
  , B.mkCacheSize

  , initialise
  , lookup
  , insert
  , clear
  , getEntries
  ) where

import           Hasura.Prelude         hiding (lookup)

import qualified Hasura.Cache.Bounded   as B
import qualified Hasura.Cache.Unbounded as U

data Cache k v
  = CacheBounded !(B.BoundedCache k v)
  | CacheUnbounded !(U.UnboundedCache k v)

newtype CacheOptions
  = CacheOptions (Maybe B.CacheSize)

mkCacheOptions :: Maybe B.CacheSize -> CacheOptions
mkCacheOptions = CacheOptions

initialise :: CacheOptions -> IO (Cache k v)
initialise (CacheOptions cacheSizeM) =
  case cacheSizeM of
    Nothing        -> CacheUnbounded <$> U.initialise
    Just cacheSize -> CacheBounded <$> B.initialise cacheSize

lookup :: (Hashable k, Ord k) => k -> Cache k v -> IO (Maybe v)
lookup k = \case
  CacheBounded cache   -> B.lookup k cache
  CacheUnbounded cache -> U.lookup k cache

insert :: (Hashable k, Ord k) => k -> v -> Cache k v -> IO ()
insert k v = \case
  CacheBounded cache   -> B.insert k v cache
  CacheUnbounded cache -> U.insert k v cache

clear :: Cache k v -> IO ()
clear = \case
  CacheBounded cache   -> B.clear cache
  CacheUnbounded cache -> U.clear cache

getEntries :: (Hashable k, Ord k) => Cache k v -> IO [[(k, v)]]
getEntries = \case
  CacheBounded cache   -> B.getEntries cache
  CacheUnbounded cache -> U.getEntries cache
