module Hasura.Cache
  ( UnboundedCache
  , initCache
  , clearCache
  , mapCache
  , insert
  , lookup
  ) where

import           Control.Concurrent  (getNumCapabilities, myThreadId,
                                      threadCapability)
import qualified Data.HashMap.Strict as Map
import qualified Data.IORef          as IORef
import qualified Data.Vector         as V

import           Hasura.Prelude      hiding (lookup)

newtype LocalCacheRef k v = LocalCacheRef (IORef.IORef (Map.HashMap k v))

mapLocalCacheRef
  :: ((k, v) -> a) -> LocalCacheRef k v -> IO [a]
mapLocalCacheRef f (LocalCacheRef ioRef) =
  map f . Map.toList <$> IORef.readIORef ioRef

-- | Create a new LC cache of the given size.
initLocalCache :: IO (LocalCacheRef k v)
initLocalCache = LocalCacheRef <$> IORef.newIORef Map.empty

clearIO :: LocalCacheRef k v -> IO ()
clearIO (LocalCacheRef ref)=
  IORef.atomicModifyIORef' ref $ const (Map.empty, ())

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insertLocal it in the cache.
lookupIO :: (Hashable k, Eq k) => LocalCacheRef k v -> k -> IO (Maybe v)
lookupIO (LocalCacheRef ref) k =
  Map.lookup k <$> IORef.readIORef ref

insertIO :: (Hashable k, Eq k) => LocalCacheRef k v -> k -> v -> IO ()
insertIO (LocalCacheRef ref) k v =
  IORef.atomicModifyIORef' ref $ \c -> (Map.insert k v c, ())

-- | Using a stripe of multiple handles can improve the performance in
-- the case of concurrent accesses since several handles can be
-- accessed in parallel.
newtype UnboundedCache k v = UnboundedCache (V.Vector (LocalCacheRef k v))

mapCache
  :: ((k, v) -> a) -> UnboundedCache k v -> IO [[a]]
mapCache f (UnboundedCache localCaches) =
  mapM (mapLocalCacheRef f) $ V.toList localCaches

-- | Create a new 'StripedHandle' with the given number of stripes and
-- the given capacity for each stripe.
initCache :: IO (UnboundedCache k v)
initCache = do
  capabilities <- getNumCapabilities
  UnboundedCache <$> V.replicateM capabilities initLocalCache

clearCache :: UnboundedCache k v -> IO ()
clearCache (UnboundedCache caches) =
  V.mapM_ clearIO caches

{-# INLINE getLocal #-}
getLocal :: UnboundedCache k v -> IO (LocalCacheRef k v)
getLocal (UnboundedCache handles) = do

  (i, _) <- myThreadId >>= threadCapability

  -- The number of capability could be dynamically changed.
  -- So, let's check the upper boundary of the vector
  let lim = V.length handles
      j | i < lim   = i
        | otherwise = i `mod` lim

  return $ handles V.! j

-- | Striped version of 'cached'.
insert
  :: (Hashable k, Eq k) => UnboundedCache k v -> k -> v -> IO ()
insert striped k v = do
  localHandle <- getLocal striped
  insertIO localHandle k v

lookup :: (Hashable k, Eq k) => UnboundedCache k v -> k -> IO (Maybe v)
lookup striped k = do
  localHandle <- getLocal striped
  lookupIO localHandle k
