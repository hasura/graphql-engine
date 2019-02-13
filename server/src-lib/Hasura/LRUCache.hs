module Hasura.LRUCache
  ( LRUCache
  , initLRUCache
  , clearLRUCache
  , insert
  , lookup
  ) where

import           Control.Concurrent  (getNumCapabilities, myThreadId,
                                      threadCapability)
import qualified Data.HashMap.Strict as Map
import qualified Data.IORef          as IORef
import qualified Data.Vector         as V
import           Data.Word           (Word64)

import           Hasura.Prelude      hiding (lookup)

----------------------------------------------

-- | Logical time at which an element was last accessed.
type Priority = Word64

newtype LocalCacheRef k v = LocalCacheRef (IORef.IORef (Map.HashMap k v))

-- | Create a new LC cache of the given size.
initLocalCache :: Word64 -> IO (LocalCacheRef k v)
initLocalCache capacity = LocalCacheRef <$> IORef.newIORef Map.empty

clearIO :: LocalCacheRef k v -> IO ()
clearIO (LocalCacheRef ref)=
  IORef.atomicModifyIORef' ref $
  -- \c -> (empty $ lcCapacity c, ())
  \c -> (Map.empty, ())

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insertLocal it in the cache.
lookupIO :: (Hashable k, Eq k) => LocalCacheRef k v -> k -> IO (Maybe v)
lookupIO (LocalCacheRef ref) k =
  -- IORef.atomicModifyIORef' ref $ \c -> case lookupLocal k c of
  -- Nothing      -> (c,  Nothing)
  -- Just (v, c') -> (c', Just v)
  Map.lookup k <$> IORef.readIORef ref

insertIO :: (Hashable k, Eq k) => LocalCacheRef k v -> k -> v -> IO ()
insertIO (LocalCacheRef ref) k v =
  -- IORef.atomicModifyIORef' ref $ \c -> (insertLocal k v c, ())
  IORef.atomicModifyIORef' ref $ \c -> (Map.insert k v c, ())

-- | Using a stripe of multiple handles can improve the performance in
-- the case of concurrent accesses since several handles can be
-- accessed in parallel.
newtype LRUCache k v = LRUCache (V.Vector (LocalCacheRef k v))

-- | Create a new 'StripedHandle' with the given number of stripes and
-- the given capacity for each stripe.
initLRUCache :: Word64 -> IO (LRUCache k v)
initLRUCache capacityPerStripe = do
  capabilities <- getNumCapabilities
  LRUCache <$> V.replicateM capabilities (initLocalCache capacityPerStripe)

clearLRUCache :: LRUCache k v -> IO ()
clearLRUCache (LRUCache caches) =
  V.mapM_ clearIO caches

{-# INLINE getLocal #-}
getLocal :: LRUCache k v -> IO (LocalCacheRef k v)
getLocal (LRUCache handles) = do

  (i, _) <- myThreadId >>= threadCapability

  -- The number of capability could be dynamically changed.
  -- So, let's check the upper boundary of the vector
  let lim = V.length handles
      j | i < lim   = i
        | otherwise = i `mod` lim

  return $ handles V.! j

-- | Striped version of 'cached'.
insert
  :: (Hashable k, Eq k) => LRUCache k v -> k -> v -> IO ()
insert striped k v = do
  localHandle <- getLocal striped
  insertIO localHandle k v

lookup :: (Hashable k, Eq k) => LRUCache k v -> k -> IO (Maybe v)
lookup striped k = do
  localHandle <- getLocal striped
  lookupIO localHandle k

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
-- lookupIO :: (Hashable k, Ord k) => LruHandle k v -> k -> IO (Maybe v)
-- lookupIO (LruHandle ref) k = do
--   m <- IORef.readIORef ref
--   return $ Map.lookup k m

-- insertIO :: (Hashable k, Ord k) => LruHandle k v -> k -> v -> IO ()
-- insertIO (LruHandle ref) k v = do
--   IORef.atomicModifyIORef' ref $ ((,()) . Map.insert k v)
