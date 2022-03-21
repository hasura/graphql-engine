module Hasura.GraphQL.Execute.Subscription.TMap
  ( TMap,
    new,
    reset,
    null,
    lookup,
    insert,
    delete,
    toList,
    replace,
    union,
    filterWithKey,
    getMap,
  )
where

import Control.Concurrent.STM
import Data.HashMap.Strict qualified as Map
import Hasura.Prelude hiding (lookup, null, toList, union)

-- | A coarse-grained transactional map implemented by simply wrapping a 'Map.HashMap' in a 'TVar'.
-- Compared to "StmContainers.Map", this provides much faster iteration over the elements at the
-- cost of significantly increased contention on writes.
newtype TMap k v = TMap {unTMap :: TVar (Map.HashMap k v)}

new :: STM (TMap k v)
new = TMap <$> newTVar Map.empty

reset :: TMap k v -> STM ()
reset = flip writeTVar Map.empty . unTMap

null :: TMap k v -> STM Bool
null = fmap Map.null . readTVar . unTMap

lookup :: (Eq k, Hashable k) => k -> TMap k v -> STM (Maybe v)
lookup k = fmap (Map.lookup k) . readTVar . unTMap

insert :: (Eq k, Hashable k) => v -> k -> TMap k v -> STM ()
insert !v k mapTv = modifyTVar' (unTMap mapTv) $ Map.insert k v

delete :: (Eq k, Hashable k) => k -> TMap k v -> STM ()
delete k mapTv = modifyTVar' (unTMap mapTv) $ Map.delete k

toList :: TMap k v -> STM [(k, v)]
toList = fmap Map.toList . readTVar . unTMap

filterWithKey :: (k -> v -> Bool) -> TMap k v -> STM ()
filterWithKey f mapTV = modifyTVar' (unTMap mapTV) $ Map.filterWithKey f

replace :: TMap k v -> Map.HashMap k v -> STM ()
replace mapTV v = void $ swapTVar (unTMap mapTV) v

union :: (Eq k, Hashable k) => TMap k v -> TMap k v -> STM (TMap k v)
union mapA mapB = do
  l <- readTVar $ unTMap mapA
  r <- readTVar $ unTMap mapB
  TMap <$> newTVar (Map.union l r)

getMap :: TMap k v -> STM (Map.HashMap k v)
getMap = readTVar . unTMap
