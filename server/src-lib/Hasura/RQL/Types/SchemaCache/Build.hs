-- | Types and functions used in the process of building the schema cache from metadata information
-- stored in the @hdb_catalog@ schema in Postgres.
module Hasura.RQL.Types.SchemaCache.Build
  ( CollectedInfo(..)
  , partitionCollectedInfo

  , CacheBuildM
  , recordInconsistency
  , recordDependencies
  , withRecordInconsistency

  , CacheRWM(..)
  , BuildReason(..)
  , buildSchemaCache
  , buildSchemaCacheFor
  , buildSchemaCacheStrict
  , withNewInconsistentObjsCheck
  ) where

import Hasura.Prelude

import qualified Data.HashMap.Strict.Extended as M
import qualified Data.Sequence as Seq

import Data.Aeson (toJSON)

import Hasura.RQL.Types.Error
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.RemoteSchema (RemoteSchemaName)
import Hasura.RQL.Types.SchemaCache

-- ----------------------------------------------------------------------------
-- types used during schema cache construction

data CollectedInfo
  = CIInconsistency !InconsistentMetadataObj
  | CIDependency
    !MetadataObject -- ^ for error reporting on missing dependencies
    !SchemaObjId
    !SchemaDependency
  deriving (Show, Eq)

partitionCollectedInfo
  :: Seq CollectedInfo
  -> ([InconsistentMetadataObj], [(MetadataObject, SchemaObjId, SchemaDependency)])
partitionCollectedInfo =
  flip foldr ([], []) \info (inconsistencies, dependencies) -> case info of
    CIInconsistency inconsistency -> (inconsistency:inconsistencies, dependencies)
    CIDependency metadataObject objectId schemaDependency ->
      let dependency = (metadataObject, objectId, schemaDependency)
      in (inconsistencies, dependency:dependencies)

type CacheBuildM = MonadWriter (Seq CollectedInfo)

recordInconsistency :: (CacheBuildM m) => MetadataObject -> Text -> m ()
recordInconsistency metadataObject reason =
  tell $ Seq.singleton $ CIInconsistency (InconsistentMetadataObj metadataObject reason)

recordDependencies :: (CacheBuildM m) => MetadataObject -> SchemaObjId -> [SchemaDependency] -> m ()
recordDependencies metadataObject schemaObjectId =
  tell . Seq.fromList . fmap (CIDependency metadataObject schemaObjectId)

withRecordInconsistency :: (QErrM m, CacheBuildM m) => MetadataObject -> m a -> m (Maybe a)
withRecordInconsistency metadataObject m = (Just <$> m) `catchError` \err -> do
  let inconsistentObject = InconsistentMetadataObj metadataObject (qeError err)
  Nothing <$ tell (Seq.singleton $ CIInconsistency inconsistentObject)

-- ----------------------------------------------------------------------------
-- operations for triggering a schema cache rebuild

class (CacheRM m) => CacheRWM m where
  buildSchemaCacheWithOptions :: BuildReason -> m ()
  invalidateCachedRemoteSchema :: RemoteSchemaName -> m ()

data BuildReason
  -- | The build was triggered by an update this instance made to the catalog (in the
  -- currently-active transaction), so information in Postgres that needs to be kept in sync with
  -- the catalog (i.e. anything in the @hdb_views@ schema) should be updated.
  = CatalogUpdate
  -- | The build was triggered by a notification that some other currently-running Hasura instance
  -- updated the catalog. Since that instance already updated @hdb_views@, this build should be
  -- read-only.
  | CatalogSync
  deriving (Show, Eq)

instance (CacheRWM m) => CacheRWM (ReaderT r m) where
  buildSchemaCacheWithOptions = lift . buildSchemaCacheWithOptions
  invalidateCachedRemoteSchema = lift . invalidateCachedRemoteSchema

buildSchemaCache :: (CacheRWM m) => m ()
buildSchemaCache = buildSchemaCacheWithOptions CatalogUpdate

-- | Rebuilds the schema cache. If an object with the given object id became newly inconsistent,
-- raises an error about it specifically. Otherwise, raises a generic metadata inconsistency error.
buildSchemaCacheFor :: (QErrM m, CacheRWM m) => MetadataObjId -> m ()
buildSchemaCacheFor objectId = do
  oldSchemaCache <- askSchemaCache
  buildSchemaCache
  newSchemaCache <- askSchemaCache

  let diffInconsistentObjects = M.differenceOn (_moId . _imoObject) `on` scInconsistentObjs
      newInconsistentObjects = newSchemaCache `diffInconsistentObjects` oldSchemaCache

  for_ (M.lookup objectId newInconsistentObjects) $ \matchingObject ->
    throw400 ConstraintViolation (_imoReason matchingObject)

  unless (null newInconsistentObjects) $
    throwError (err400 Unexpected "cannot continue due to new inconsistent metadata")
      { qeInternal = Just $ toJSON (M.elems newInconsistentObjects) }

-- | Like 'buildSchemaCache', but fails if there is any inconsistent metadata.
buildSchemaCacheStrict :: (QErrM m, CacheRWM m) => m ()
buildSchemaCacheStrict = do
  buildSchemaCache
  sc <- askSchemaCache
  let inconsObjs = scInconsistentObjs sc
  unless (null inconsObjs) $ do
    let err = err400 Unexpected "cannot continue due to inconsistent metadata"
    throwError err{ qeInternal = Just $ toJSON inconsObjs }

-- | Executes the given action, and if any new 'InconsistentMetadataObj's are added to the schema
-- cache as a result of its execution, raises an error.
withNewInconsistentObjsCheck :: (QErrM m, CacheRM m) => m a -> m a
withNewInconsistentObjsCheck action = do
  originalObjects <- scInconsistentObjs <$> askSchemaCache
  result <- action
  currentObjects <- scInconsistentObjs <$> askSchemaCache

  let newInconsistentObjects =
        M.elems $ M.differenceOn (_moId . _imoObject) currentObjects originalObjects
  unless (null newInconsistentObjects) $
    throwError (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
      { qeInternal = Just $ toJSON newInconsistentObjects }

  pure result
