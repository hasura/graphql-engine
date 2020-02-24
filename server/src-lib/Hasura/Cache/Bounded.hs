{-| An in-memory, Bounded by LRU strategy, capability-local cache implementation.
By making the cache capability-local, data may be recomputed up to once per
capability (which usually means up to once per OS thread), but write contention
from multiple threads is unlikely.
-}
module Hasura.Cache.Bounded
  ( BoundedCache
  , CacheSize
  , mkCacheSize

  , initialise
  , clear
  , insert
  , insertAllStripes
  , lookup
  , getEntries
  ) where

import           Hasura.Prelude     hiding (lookup)

import           Control.Concurrent (getNumCapabilities, myThreadId, threadCapability)
import           Data.Word          (Word16)

import qualified Data.Aeson         as J
import qualified Data.HashPSQ       as HashPSQ
import qualified Data.IORef         as IORef
import qualified Data.Vector        as V
import           GHC.Natural        (Natural)

newtype CacheSize
  = CacheSize { unCacheSize :: Word16 }
  deriving (Show, Read, Eq, Ord, Bounded, Num, Enum, J.ToJSON, J.FromJSON)

mkCacheSize :: String -> Either String CacheSize
mkCacheSize v =
  -- NOTE: naively using readMaybe Word16 will silently wrap
  case readMaybe v :: Maybe Natural of
    Just n | n <= max16 && n > 0 -> return (CacheSize $ fromIntegral n)
    _ -> fail "cache size must be given as a number between 1 and 65535"
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
  -- ^ The current number of elements in the cache
  , _lcTick     :: !Tick
  -- ^ The priority is drawn from this tick, it is incremented
  -- after every access
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

-- | Restore 'LruCache' invariants
trim :: (Hashable k, Ord k) => LruCache k v -> LruCache k v
trim c
  | _lcTick c == maxBound     = emptyCache (_lcCapacity c)
  | _lcSize c > _lcCapacity c =
      c  { _lcSize  = _lcSize c - 1
         , _lcQueue = HashPSQ.deleteMin (_lcQueue c)
         }
  | otherwise                 = c

-- | Insert an element into the 'LruCache'.
insertPure :: (Hashable k, Ord k) => k -> v -> LruCache k v -> LruCache k v
insertPure key val c =
  trim $!
  let (mbOldVal,queue) = HashPSQ.insertView key (_lcTick c) val (_lcQueue c)
  in c  { _lcSize  = if isNothing mbOldVal
                     then _lcSize c + 1
                     else _lcSize c
        , _lcTick  = _lcTick c + 1
        , _lcQueue = queue
        }

-- | Lookup an element in an 'LruCache' and mark it as the least
-- recently accessed.
lookupPure :: (Hashable k, Ord k) => k -> LruCache k v -> Maybe (v, LruCache k v)
lookupPure k c =
  case HashPSQ.alter lookupAndBump k (_lcQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  ->
      let !c' = trim $ c {_lcTick = _lcTick c + 1, _lcQueue = q}
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

getEntries
  :: (Hashable k, Ord k)
  => BoundedCache k v -> IO [[(k, v)]]
getEntries (BoundedCache localCaches) =
  mapM getLocalEntries $ V.toList localCaches

-- | Creates a new BoundedCache of the specified size,
-- with one stripe per capability
initialise :: CacheSize -> IO (BoundedCache k v)
initialise capacity = do
  capabilities <- getNumCapabilities
  BoundedCache <$> V.replicateM capabilities (initLocalCache capacity)

clear :: BoundedCache k v -> IO ()
clear (BoundedCache caches) =
  V.mapM_ clearLocal caches

{-# INLINE getLocal #-}
getLocal :: BoundedCache k v -> IO (LocalCacheRef k v)
getLocal (BoundedCache handles) = do

  (i, _) <- myThreadId >>= threadCapability

  -- The number of capabilities can grow dynamically so make sure we wrap
  -- around when indexing.
  let j = i `mod` V.length handles

  return $ handles V.! j

-- | Insert into our thread's local cache stripe.
insert
  :: (Hashable k, Ord k) => k -> v -> BoundedCache k v -> IO ()
insert k v striped = do
  localHandle <- getLocal striped
  insertLocal localHandle k v

-- | Insert into all stripes (non-atomically).
insertAllStripes
  :: (Hashable k, Ord k) =>  k -> v -> BoundedCache k v ->IO ()
insertAllStripes k v (BoundedCache handles) = do
  forM_ handles $ \localHandle->
    insertLocal localHandle k v

lookup :: (Hashable k, Ord k) => k -> BoundedCache k v -> IO (Maybe v)
lookup k striped = do
  localHandle <- getLocal striped
  lookupLocal localHandle k
