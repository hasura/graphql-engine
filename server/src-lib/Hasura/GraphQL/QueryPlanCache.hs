{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Hasura.GraphQL.QueryPlanCache
  ( QueryPlanCache
  , initQueryPlanCache
  , getQueryPlan
  , addQueryPlan
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
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import qualified Hasura.GraphQL.Plan                    as EP
import           Hasura.Prelude
import           Hasura.RQL.Types

type QueryPlanCache =
  StripedLruHandle (RoleName, Maybe GH.OperationName, GH.GraphQLQuery) EP.QueryPlan

getQueryPlan
  :: RoleName -> GH.GraphQLRequest -> QueryPlanCache -> IO (Maybe EP.QueryPlan)
getQueryPlan rn (GH.GraphQLRequest opNameM q _) planCache =
  lookupStriped planCache (rn, opNameM, q)

addQueryPlan
  :: RoleName -> GH.GraphQLRequest -> EP.QueryPlan -> QueryPlanCache -> IO ()
addQueryPlan rn (GH.GraphQLRequest opNameM q _) queryPlan cache =
  insertStriped cache (rn, opNameM, q) queryPlan

initQueryPlanCache :: IO QueryPlanCache
initQueryPlanCache = do
  n <- getNumCapabilities
  newStripedLruHandle n 100

----------------------------------------------

-- | Logical time at which an element was last accessed.
type Priority = Word64

-- | LRU cache based on hashing.
data LruCache k v = LruCache
  { lruCapacity :: !Word64
  , lruSize     :: !Word64
  , lruTick     :: !Priority
  , lruQueue    :: !(HashPSQ.HashPSQ k Priority v)
  } deriving (Eq,Show,Functor,Foldable,Traversable)


-- | Create an empty 'LruCache' of the given size.
empty :: Word64 -> LruCache k v
empty capacity =
      LruCache
        { lruCapacity = capacity
        , lruSize     = 0
        , lruTick     = 0
        , lruQueue    = HashPSQ.empty
        }

trim :: (Hashable k, Ord k) => LruCache k v -> LruCache k v
trim c
  | lruTick c == maxBound     = empty (lruCapacity c)
  | lruSize c > lruCapacity c =
      c  { lruSize  = lruSize c - 1
         , lruQueue = HashPSQ.deleteMin (lruQueue c)
         }
  | otherwise                 = c

-- | Insert an element into the 'LruCache'.
insert :: (Hashable k, Ord k) => k -> v -> LruCache k v -> LruCache k v
insert key val c =
  trim $!
  let (mbOldVal,queue) = HashPSQ.insertView key (lruTick c) val (lruQueue c)
  in c  { lruSize  = if isNothing mbOldVal
                     then lruSize c + 1
                     else lruSize c
        , lruTick  = lruTick c + 1
        , lruQueue = queue
        }

-- | Lookup an element in an 'LruCache' and mark it as the least
-- recently accessed.
lookup :: (Hashable k, Ord k) => k -> LruCache k v -> Maybe (v, LruCache k v)
lookup k c =
  case HashPSQ.alter lookupAndBump k (lruQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  ->
      let !c' = trim $ c {lruTick = lruTick c + 1, lruQueue = q}
      in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x, Just (lruTick c, x))

-- | Store a LRU cache in an 'IORef to be able to conveniently update it.
newtype LruHandle k v = LruHandle (IORef.IORef (LruCache k v))

-- | Create a new LRU cache of the given size.
newLruHandle :: Word64 -> IO (LruHandle k v)
newLruHandle capacity = LruHandle <$> IORef.newIORef (empty capacity)

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
lookupIO :: (Hashable k, Ord k) => LruHandle k v -> k -> IO (Maybe v)
lookupIO (LruHandle ref) k =
  IORef.atomicModifyIORef' ref $ \c -> case lookup k c of
  Nothing      -> (c,  Nothing)
  Just (v, c') -> (c', Just v)

insertIO :: (Hashable k, Ord k) => LruHandle k v -> k -> v -> IO ()
insertIO (LruHandle ref) k v =
  IORef.atomicModifyIORef' ref $ \c -> (insert k v c, ())

-- | Using a stripe of multiple handles can improve the performance in
-- the case of concurrent accesses since several handles can be
-- accessed in parallel.
newtype StripedLruHandle k v = StripedLruHandle (V.Vector (LruHandle k v))

-- | Create a new 'StripedHandle' with the given number of stripes and
-- the given capacity for each stripe.
newStripedLruHandle :: Int -> Word64 -> IO (StripedLruHandle k v)
newStripedLruHandle numStripes capacityPerStripe =
  StripedLruHandle <$> V.replicateM numStripes (newLruHandle capacityPerStripe)

{-# INLINE getLocal #-}
getLocal :: StripedLruHandle k v -> IO (LruHandle k v)
getLocal (StripedLruHandle handles) = do

  (i, _) <- myThreadId >>= threadCapability

  -- The number of capability could be dynamically changed.
  -- So, let's check the upper boundary of the vector
  let lim = V.length handles
      j | i < lim   = i
        | otherwise = i `mod` lim

  return $ handles V.! j

-- | Striped version of 'cached'.
insertStriped
  :: (Hashable k, Ord k) => StripedLruHandle k v -> k -> v -> IO ()
insertStriped striped k v = do
  localHandle <- getLocal striped
  insertIO localHandle k v

lookupStriped :: (Hashable k, Ord k) => StripedLruHandle k v -> k -> IO (Maybe v)
lookupStriped striped k = do
  localHandle <- getLocal striped
  lookupIO localHandle k
