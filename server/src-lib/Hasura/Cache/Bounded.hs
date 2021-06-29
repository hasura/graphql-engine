{-| An in-memory, Bounded by LRU strategy, capability-local cache implementation.
By making the cache capability-local, data may be recomputed up to once per
capability (which usually means up to once per OS thread), but write contention
from multiple threads is unlikely.
-}
module Hasura.Cache.Bounded
  ( BoundedCache
  , CacheSize(..)
  , parseCacheSize

  , initialise
  , initialiseStripes
  , insertAllStripes
  , lookup
  , insert
  , clear
  , getEntries

  -- * Exposed for testing
  , checkInvariants
  , getEntriesRecency
  ) where

import           Hasura.Prelude     hiding (lookup)

import qualified Data.Aeson         as J
import qualified Data.HashPSQ       as HashPSQ
import qualified Data.IORef         as IORef
import qualified Data.Vector        as V

import           Control.Concurrent (getNumCapabilities, myThreadId, threadCapability)
import           Data.Word          (Word16)
import           GHC.Natural        (Natural)


-- MISC TODO:
--  - benchmark and consider unsafeLookupIncreasePriority and unsafeInsertIncreasePriorityView
--  - our own concurrent cache, that doesn't need redundant stripes
--    - this would save significant memory
--    - we could choose something more sophisticated than LRU if warranted
--    - we could probably keep things simple by evicting on hash collisions

newtype CacheSize
  = CacheSize { unCacheSize :: Word16 }
  deriving (Show, Read, Eq, Ord, Bounded, Num, Real, Integral, Enum, J.ToJSON, J.FromJSON)

parseCacheSize :: String -> Either String CacheSize
parseCacheSize v =
  -- NOTE: naively using readMaybe Word16 will silently wrap
  case readMaybe v :: Maybe Natural of
    Just n | n <= max16 && n >= 0 -> return (CacheSize $ fromIntegral n)
    _ -> throwError "cache size must be given as a number between 0 and 65535"
  where
    max16 = fromIntegral (maxBound :: Word16) :: Natural

newtype Tick
  = Tick { unTick :: Word64 }
  deriving (Show, Eq, Ord, Hashable, Bounded, Num, Enum)

-- | LRU cache based on hashing
-- Based on https://hackage.haskell.org/package/lrucaching
data LruCache k v = LruCache
  { _lcCapacity :: !CacheSize
  -- ^ The maximum number of elements in the cache
  , _lcSize     :: !CacheSize
  -- ^ The current number of elements in the cache. We maintain this ourselves
  -- since 'HashPSQ.size' is O(n).
  , _lcTick     :: !Tick
  -- ^ The priority is drawn from this tick, it is incremented
  -- after insert or successful lookup
  , _lcQueue    :: !(HashPSQ.HashPSQ k Tick v)
  -- ^ Underlying priority queue
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Create an empty 'LruCache' of the given size.
emptyCache :: CacheSize -> LruCache k v
emptyCache capacity =
  LruCache
  { _lcCapacity = capacity
  , _lcSize     = 0
  , _lcTick     = Tick 0
  , _lcQueue    = HashPSQ.empty
  }

-- | Insert an element into the 'LruCache'.
insertPure :: (Hashable k, Ord k) => k -> v -> LruCache k v -> LruCache k v
insertPure key val c =
  -- NOTE: we assume any rollover of 64-bit counters here to be impossible:
  let (mbOldVal,queue) = HashPSQ.insertView key (_lcTick c) val (_lcQueue c)
      cTicked = c{ _lcTick = _lcTick c + 1 }
   in case mbOldVal of
        Nothing
          -- at capacity; remove LRU to maintain _lcSize:
          | _lcSize c == _lcCapacity c ->
               cTicked{ _lcQueue = HashPSQ.deleteMin queue }
          -- under capacity; just record new size after insert:
          | otherwise ->
               cTicked{ _lcQueue = queue, _lcSize  = _lcSize c + 1 }
        _valueAlreadyInserted -> -- nothing inserted; _lcSize stays the same
               cTicked{ _lcQueue = queue }

-- | Lookup an element in an 'LruCache' and mark it as the least
-- recently accessed.
lookupPure :: (Hashable k, Ord k) => k -> LruCache k v -> Maybe (v, LruCache k v)
lookupPure k c =
  case HashPSQ.alter lookupAndBump k (_lcQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  ->
      let !c' = c {_lcTick = _lcTick c + 1, _lcQueue = q}
      in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just (_lcTick c, x))

newtype LocalCacheRef k v = LocalCacheRef (IORef.IORef (LruCache k v))

getLocalEntries
  :: (Hashable k, Ord k) => LocalCacheRef k v -> IO [(k, v)]
getLocalEntries (LocalCacheRef ioRef) =
  map (\(k, _, v) -> (k, v)) . HashPSQ.toList . _lcQueue
  <$> IORef.readIORef ioRef

-- | Create a new Local cache of the given size.
initLocalCache :: CacheSize -> IO (LocalCacheRef k v)
initLocalCache capacity = LocalCacheRef <$> IORef.newIORef (emptyCache capacity)

-- | clear a local cache
clearLocal :: LocalCacheRef k v -> IO ()
clearLocal (LocalCacheRef ref)=
  IORef.atomicModifyIORef' ref $
  \currentCache -> (emptyCache (_lcCapacity currentCache), ())

-- | lookup for a key in the local cache
lookupLocal :: (Hashable k, Ord k) => LocalCacheRef k v -> k -> IO (Maybe v)
lookupLocal (LocalCacheRef ref) k =
  -- Return the result and replace the cache if needed
  IORef.atomicModifyIORef' ref $ \currentCache ->
    case lookupPure k currentCache of
      Just (v, newCache) -> (newCache, Just v)
      Nothing            -> (currentCache, Nothing)

-- | insert into a local cache
insertLocal :: (Hashable k, Ord k) => LocalCacheRef k v -> k -> v -> IO ()
insertLocal (LocalCacheRef ref) k v =
  IORef.atomicModifyIORef' ref $ \c -> (insertPure k v c, ())

-- | Using a stripe of multiple handles can improve the performance in
-- the case of concurrent accesses since several handles can be
-- accessed in parallel.
newtype BoundedCache k v = BoundedCache (V.Vector (LocalCacheRef k v))



lookup :: (Hashable k, Ord k) => k -> BoundedCache k v -> IO (Maybe v)
lookup k striped = do
  localHandle <- getLocal striped
  lookupLocal localHandle k

insert :: (Hashable k, Ord k) => k -> v -> BoundedCache k v -> IO ()
insert k v striped = do
  localHandle <- getLocal striped
  insertLocal localHandle k v

clear :: BoundedCache k v -> IO ()
clear (BoundedCache caches) =
  V.mapM_ clearLocal caches

getEntries :: (Hashable k, Ord k)=> BoundedCache k v -> IO [[(k, v)]]
getEntries (BoundedCache localCaches) =
  mapM getLocalEntries $ V.toList localCaches

-- | Creates a new BoundedCache of the specified size, with one stripe per capability.
initialise :: CacheSize -> IO (BoundedCache k v)
initialise sz = do
  caps <- getNumCapabilities
  initialiseStripes caps sz

-- | Creates a new BoundedCache of the specified size, for each stripe
initialiseStripes
  :: Int
  -- ^ Stripes; to minimize contention this should probably match the number of capabilities.
  -> CacheSize
  -> IO (BoundedCache k v)
initialiseStripes stripes capacity = do
  BoundedCache <$> V.replicateM stripes (initLocalCache capacity)


{-# INLINE getLocal #-}
getLocal :: BoundedCache k v -> IO (LocalCacheRef k v)
getLocal (BoundedCache handles) = do

  (i, _) <- myThreadId >>= threadCapability

  -- The number of capabilities can grow dynamically so make sure we wrap
  -- around when indexing.
  let j = i `mod` V.length handles

  return $ handles V.! j

-- | Insert into all stripes (non-atomically).
insertAllStripes
  :: (Hashable k, Ord k) =>  k -> v -> BoundedCache k v ->IO ()
insertAllStripes k v (BoundedCache handles) = do
  forM_ handles $ \localHandle->
    insertLocal localHandle k v

-- | Check internal invariants, throwing an error if things are off:
checkInvariants :: (Hashable k, Ord k)=> BoundedCache k v -> IO ()
checkInvariants (BoundedCache handles) =
  forM_ handles $ \(LocalCacheRef ref) -> do
    LruCache{..} <- IORef.readIORef ref
    -- check internal invariants (in case we use unsafe* functions that rely on
    -- us maintaining HashPSQ invariants):
    unless (HashPSQ.valid _lcQueue) $
      error "invalid HashPSQ!"
    unless (HashPSQ.size _lcQueue == fromIntegral _lcSize) $
      error "Size incorrect!"
    unless (fromIntegral _lcSize <= _lcTick)$
      error "Somehow tick wasn't incremented properly!"
    when (_lcSize > _lcCapacity) $
      error "Size > capacity!"

getEntriesRecency :: (Hashable k, Ord k) => BoundedCache k v -> IO [[(k, Tick, v)]]
getEntriesRecency (BoundedCache localCaches) =
  forM (V.toList localCaches) $ \(LocalCacheRef ref) ->
    HashPSQ.toList . _lcQueue <$> IORef.readIORef ref
