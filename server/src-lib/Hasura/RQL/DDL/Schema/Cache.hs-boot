module Hasura.RQL.DDL.Schema.Cache where

import           Hasura.Prelude

import           Hasura.Db
import           Hasura.RQL.Types

withMetadataCheck :: (MonadTx m, CacheRWM m) => Bool -> m a -> m a
purgeDependentObject :: (MonadTx m) => SchemaObjId -> m ()
