{-| An in-memory, unbounded, capability-local cache implementation. By making the cache
capability-local, data may be recomputed up to once per capability (which usually means up to once
per OS thread), but write contention from multiple threads is unlikely. -}
module Hasura.Cache.Unbounded
  ( UnboundedCache
  , initialise
  , insertAllStripes
  ) where

import           Hasura.Prelude      hiding (lookup)
import           Hasura.Cache.Types

import           Control.Concurrent  (getNumCapabilities, myThreadId, threadCapability)

import qualified Data.HashMap.Strict as Map
import qualified Data.IORef          as IORef
import qualified Data.Vector         as V

newtype LocalCacheRef k v = LocalCacheRef (IORef.IORef (Map.HashMap k v))

getEntriesLocal
  :: LocalCacheRef k v -> IO [(k, v)]
getEntriesLocal (LocalCacheRef ioRef) =
  Map.toList <$> IORef.readIORef ioRef

-- | Create a new LC cache of the given size.
initialiseLocal :: IO (LocalCacheRef k v)
initialiseLocal = LocalCacheRef <$> IORef.newIORef Map.empty

clearLocal :: LocalCacheRef k v -> IO ()
clearLocal (LocalCacheRef ref)=
  IORef.atomicModifyIORef' ref $ const (Map.empty, ())

lookupLocal :: (Hashable k, Eq k) => LocalCacheRef k v -> k -> IO (Maybe v)
lookupLocal (LocalCacheRef ref) k =
  Map.lookup k <$> IORef.readIORef ref

insertLocal :: (Hashable k, Eq k) => LocalCacheRef k v -> k -> v -> IO ()
insertLocal (LocalCacheRef ref) k v =
  IORef.atomicModifyIORef' ref $ \c -> (Map.insert k v c, ())

-- | Using a stripe of multiple handles can improve the performance in
-- the case of concurrent accesses since several handles can be
-- accessed in parallel.
newtype UnboundedCache k v = UnboundedCache (V.Vector (LocalCacheRef k v))

instance (Hashable k, Ord k) => CacheObj (UnboundedCache k v) k v where
  lookup k striped = do
    localHandle <- getLocal striped
    lookupLocal localHandle k
  insert k v striped = do
    localHandle <- getLocal striped
    insertLocal localHandle k v
  clear (UnboundedCache caches) =
    V.mapM_ clearLocal caches
  getEntries (UnboundedCache localCaches) =
    mapM getEntriesLocal $ V.toList localCaches

-- | Create a new 'StripedHandle' with the given number of stripes and
-- the given capacity for each stripe.
initialise :: IO (UnboundedCache k v)
initialise = do
  capabilities <- getNumCapabilities
  UnboundedCache <$> V.replicateM capabilities initialiseLocal

{-# INLINE getLocal #-}
getLocal :: UnboundedCache k v -> IO (LocalCacheRef k v)
getLocal (UnboundedCache handles) = do

  (i, _) <- myThreadId >>= threadCapability

  -- The number of capabilities can grow dynamically so make sure we wrap
  -- around when indexing.
  let j = i `mod` V.length handles

  return $ handles V.! j

-- | Insert into all stripes (non-atomically).
insertAllStripes
  :: (Hashable k, Eq k) =>  k -> v -> UnboundedCache k v ->IO ()
insertAllStripes k v (UnboundedCache handles) = do
  forM_ handles $ \localHandle->
    insertLocal localHandle k v
