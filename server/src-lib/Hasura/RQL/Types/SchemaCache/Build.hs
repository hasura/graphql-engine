{-# LANGUAGE Arrows #-}

-- | Types and functions used in the process of building the schema cache from metadata information
-- stored in the @hdb_catalog@ schema in Postgres.
module Hasura.RQL.Types.SchemaCache.Build
  ( CollectedInfo(..)
  , partitionCollectedInfo

  , recordInconsistency
  , recordInconsistencies
  , recordDependencies
  , withRecordInconsistency

  , CacheRWM(..)
  , BuildReason(..)
  , buildSchemaCache
  , buildSchemaCacheFor
  , buildSchemaCacheStrict
  , withNewInconsistentObjsCheck
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as M
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T

import           Control.Arrow.Extended
import           Data.Aeson                    (toJSON)
import           Data.List                     (nub)

import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Metadata
import           Hasura.RQL.Types.RemoteSchema (RemoteSchemaName)
import           Hasura.RQL.Types.SchemaCache

-- ----------------------------------------------------------------------------
-- types used during schema cache construction

data CollectedInfo
  = CIInconsistency !InconsistentMetadata
  | CIDependency
    !MetadataObject -- ^ for error reporting on missing dependencies
    !SchemaObjId
    !SchemaDependency
  deriving (Show, Eq)

partitionCollectedInfo
  :: Seq CollectedInfo
  -> ([InconsistentMetadata], [(MetadataObject, SchemaObjId, SchemaDependency)])
partitionCollectedInfo =
  flip foldr ([], []) \info (inconsistencies, dependencies) -> case info of
    CIInconsistency inconsistency -> (inconsistency:inconsistencies, dependencies)
    CIDependency metadataObject objectId schemaDependency ->
      let dependency = (metadataObject, objectId, schemaDependency)
      in (inconsistencies, dependency:dependencies)

recordInconsistency :: (ArrowWriter (Seq CollectedInfo) arr) => (MetadataObject, Text) `arr` ()
recordInconsistency = first (arr (:[])) >>> recordInconsistencies

recordInconsistencies :: (ArrowWriter (Seq CollectedInfo) arr) => ([MetadataObject], Text) `arr` ()
recordInconsistencies = proc (metadataObjects, reason) ->
  tellA -< Seq.fromList $ map (CIInconsistency . InconsistentObject reason) metadataObjects

recordDependencies
  :: (ArrowWriter (Seq CollectedInfo) arr)
  => (MetadataObject, SchemaObjId, [SchemaDependency]) `arr` ()
recordDependencies = proc (metadataObject, schemaObjectId, dependencies) ->
  tellA -< Seq.fromList $ map (CIDependency metadataObject schemaObjectId) dependencies

withRecordInconsistency
  :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
  => ErrorA QErr arr (e, s) a
  -> arr (e, (MetadataObject, s)) (Maybe a)
withRecordInconsistency f = proc (e, (metadataObject, s)) -> do
  result <- runErrorA f -< (e, s)
  case result of
    Left err -> do
      recordInconsistency -< (metadataObject, qeError err)
      returnA -< Nothing
    Right v -> returnA -< Just v
{-# INLINABLE withRecordInconsistency #-}

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

  let diffInconsistentObjects = M.difference `on` (groupInconsistentMetadataById . scInconsistentObjs)
      newInconsistentObjects = newSchemaCache `diffInconsistentObjects` oldSchemaCache

  for_ (M.lookup objectId newInconsistentObjects) $ \matchingObjects -> do
    let reasons = T.intercalate ", " $ map imReason $ toList matchingObjects
    throwError (err400 ConstraintViolation reasons) { qeInternal = Just $ toJSON matchingObjects }

  unless (null newInconsistentObjects) $
    throwError (err400 Unexpected "cannot continue due to new inconsistent metadata")
      { qeInternal = Just $ toJSON (nub . concatMap toList $ M.elems newInconsistentObjects) }

-- | Like 'buildSchemaCache', but fails if there is any inconsistent metadata.
buildSchemaCacheStrict :: (QErrM m, CacheRWM m) => m ()
buildSchemaCacheStrict = do
  buildSchemaCache
  sc <- askSchemaCache
  let inconsObjs = scInconsistentObjs sc
  unless (null inconsObjs) $ do
    let err = err400 Unexpected "cannot continue due to inconsistent metadata"
    throwError err{ qeInternal = Just $ toJSON inconsObjs }

-- | Executes the given action, and if any new 'InconsistentMetadata's are added to the schema
-- cache as a result of its execution, raises an error.
withNewInconsistentObjsCheck :: (QErrM m, CacheRM m) => m a -> m a
withNewInconsistentObjsCheck action = do
  originalObjects <- scInconsistentObjs <$> askSchemaCache
  result <- action
  currentObjects <- scInconsistentObjs <$> askSchemaCache

  let diffInconsistentObjects = M.difference `on` groupInconsistentMetadataById
      newInconsistentObjects =
        nub $ concatMap toList $ M.elems (currentObjects `diffInconsistentObjects` originalObjects)
  unless (null newInconsistentObjects) $
    throwError (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
      { qeInternal = Just $ toJSON newInconsistentObjects }

  pure result
