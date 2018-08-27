{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Hasura.GraphQL.LRUCache
  ( LRUCache
  , initLRUCache
  , clearLRUCache
  , lookup
  , insert
  ) where

import           Control.Concurrent                     (getNumCapabilities,
                                                         myThreadId,
                                                         threadCapability)
import           Data.Foldable                          (Foldable)
import qualified Data.HashPSQ                           as HashPSQ
import qualified Data.IORef                             as IORef
import           Data.Maybe                             (isNothing)
import           Data.Traversable                       (Traversable)
import qualified Data.Vector                            as V
import           Data.Word                              (Word64)

import           Hasura.Prelude

----------------------------------------------

-- | Logical time at which an element was last accessed.
type Priority = Word64

-- | LC cache based on hashing.
data LocalCache k v
  = LocalCache
  { lcCapacity :: !Word64
  , lcSize     :: !Word64
  , lcTick     :: !Priority
  , lcQueue    :: !(HashPSQ.HashPSQ k Priority v)
  } deriving (Eq,Show,Functor,Foldable,Traversable)

-- | Create an empty 'LruCache' of the given size.
empty
  :: Word64 -> LocalCache k v
empty capacity =
  LocalCache
  { lcCapacity = capacity
  , lcSize     = 0
  , lcTick     = 0
  , lcQueue    = HashPSQ.empty
  }

trim :: (Hashable k, Ord k) => LocalCache k v -> LocalCache k v
trim c
  | lcTick c == maxBound     = empty (lcCapacity c)
  | lcSize c > lcCapacity c =
      c  { lcSize  = lcSize c - 1
         , lcQueue = HashPSQ.deleteMin (lcQueue c)
         }
  | otherwise                 = c

-- | Insert an element into the 'LruCache'.
insertLocal :: (Hashable k, Ord k) => k -> v -> LocalCache k v -> LocalCache k v
insertLocal key val c =
  trim $!
  let (mbOldVal,queue) = HashPSQ.insertView key (lcTick c) val (lcQueue c)
  in c  { lcSize  = if isNothing mbOldVal
                     then lcSize c + 1
                     else lcSize c
        , lcTick  = lcTick c + 1
        , lcQueue = queue
        }

-- | Lookup an element in an 'LruCache' and mark it as the least
-- recently accessed.
lookupLocal :: (Hashable k, Ord k) => k -> LocalCache k v -> Maybe (v, LocalCache k v)
lookupLocal k c =
  case HashPSQ.alter lookupAndBump k (lcQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  ->
      let !c' = trim $ c { lcTick = lcTick c + 1, lcQueue = q}
      in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x, Just (lcTick c, x))

-- | Store a LC cache in an 'IORef to be able to conveniently update it.
newtype LocalCacheRef k v = LocalCacheRef (IORef.IORef (LocalCache k v))

-- | Create a new LC cache of the given size.
initLocalCache :: Word64 -> IO (LocalCacheRef k v)
initLocalCache capacity = LocalCacheRef <$> IORef.newIORef (empty capacity)

clearIO :: LocalCacheRef k v -> IO ()
clearIO (LocalCacheRef ref)=
  IORef.atomicModifyIORef' ref $
  \c -> (empty $ lcCapacity c, ())

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insertLocal it in the cache.
lookupIO :: (Hashable k, Ord k) => LocalCacheRef k v -> k -> IO (Maybe v)
lookupIO (LocalCacheRef ref) k =
  IORef.atomicModifyIORef' ref $ \c -> case lookupLocal k c of
  Nothing      -> (c,  Nothing)
  Just (v, c') -> (c', Just v)

insertIO :: (Hashable k, Ord k) => LocalCacheRef k v -> k -> v -> IO ()
insertIO (LocalCacheRef ref) k v =
  IORef.atomicModifyIORef' ref $ \c -> (insertLocal k v c, ())

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
  :: (Hashable k, Ord k) => LRUCache k v -> k -> v -> IO ()
insert striped k v = do
  localHandle <- getLocal striped
  insertIO localHandle k v

lookup :: (Hashable k, Ord k) => LRUCache k v -> k -> IO (Maybe v)
lookup striped k = do
  localHandle <- getLocal striped
  lookupIO localHandle k
