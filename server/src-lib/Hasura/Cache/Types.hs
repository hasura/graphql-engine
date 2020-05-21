{-# LANGUAGE ExistentialQuantification #-}

module Hasura.Cache.Types
  ( CacheObj(..)
  , Cache(..)
  ) where

import           Hasura.Prelude         hiding (lookup)

class (Hashable k, Ord k) => CacheObj c k v | c -> k v where
  lookup :: k -> c -> IO (Maybe v)
  insert :: k -> v -> c -> IO ()
  clear :: c -> IO ()
  getEntries :: c -> IO [[(k, v)]]

data Cache k v = forall c . CacheObj c k v => Cache c

instance (Hashable k, Ord k) => CacheObj (Cache k v) k v where
  lookup k (Cache c) = lookup k c
  insert k v (Cache c) = insert k v c
  clear (Cache c) = clear c
  getEntries (Cache c) = getEntries c
