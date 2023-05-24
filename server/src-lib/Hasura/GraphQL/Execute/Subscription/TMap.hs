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
    adjust,
  )
where

import Control.Concurrent.STM
import Data.HashMap.Strict qualified as HashMap
import Hasura.Prelude hiding (lookup, null, toList, union)

-- | A coarse-grained transactional map implemented by simply wrapping a 'HashMap.HashMap' in a 'TVar'.
-- Compared to "StmContainers.Map", this provides much faster iteration over the elements at the
-- cost of significantly increased contention on writes.
newtype TMap k v = TMap {unTMap :: TVar (HashMap.HashMap k v)}

new :: STM (TMap k v)
new = TMap <$> newTVar HashMap.empty

reset :: TMap k v -> STM ()
reset = flip writeTVar HashMap.empty . unTMap

null :: TMap k v -> STM Bool
null = fmap HashMap.null . readTVar . unTMap

lookup :: (Hashable k) => k -> TMap k v -> STM (Maybe v)
lookup k = fmap (HashMap.lookup k) . readTVar . unTMap

insert :: (Hashable k) => v -> k -> TMap k v -> STM ()
insert !v k mapTv = modifyTVar' (unTMap mapTv) $ HashMap.insert k v

delete :: (Hashable k) => k -> TMap k v -> STM ()
delete k mapTv = modifyTVar' (unTMap mapTv) $ HashMap.delete k

toList :: TMap k v -> STM [(k, v)]
toList = fmap HashMap.toList . readTVar . unTMap

filterWithKey :: (k -> v -> Bool) -> TMap k v -> STM ()
filterWithKey f mapTV = modifyTVar' (unTMap mapTV) $ HashMap.filterWithKey f

replace :: TMap k v -> HashMap.HashMap k v -> STM ()
replace mapTV v = void $ swapTVar (unTMap mapTV) v

union :: (Hashable k) => TMap k v -> TMap k v -> STM (TMap k v)
union mapA mapB = do
  l <- readTVar $ unTMap mapA
  r <- readTVar $ unTMap mapB
  TMap <$> newTVar (HashMap.union l r)

getMap :: TMap k v -> STM (HashMap.HashMap k v)
getMap = readTVar . unTMap

adjust :: (Hashable k) => (v -> v) -> k -> TMap k v -> STM ()
adjust f k mapTV = modifyTVar' (unTMap mapTV) $ HashMap.adjust f k
