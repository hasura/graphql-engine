module Hasura.RQL.DDL.Schema.Cache where

import           Hasura.Prelude

import           Hasura.Db
import           Hasura.RQL.Types

type CacheBuildM m = (CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)

buildSchemaCacheStrict :: (CacheBuildM m) => m ()
buildSchemaCacheFor :: (CacheBuildM m) => MetadataObjId -> m ()
buildSchemaCache :: (CacheBuildM m) => m ()
buildSchemaCacheWithoutSetup :: (CacheBuildM m) => m ()

withNewInconsistentObjsCheck :: (QErrM m, CacheRM m) => m a -> m a
withMetadataCheck :: (CacheBuildM m) => Bool -> m a -> m a
purgeDependentObject :: (CacheRWM m, MonadTx m) => SchemaObjId -> m ()

withSchemaObject :: (QErrM m, CacheRWM m) => (Text -> InconsistentMetadataObj) -> m a -> m (Maybe a)
withSchemaObject_ :: (QErrM m, CacheRWM m) => (Text -> InconsistentMetadataObj) -> m () -> m ()
