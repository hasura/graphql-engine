{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types/functions shared between modules that implement "Hasura.RQL.DDL.Schema.Cache". Other
-- modules should not import this module directly.
module Hasura.RQL.DDL.Schema.Cache.Common where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended     as M
import qualified Data.HashSet                     as HS
import qualified Data.Sequence                    as Seq

import           Control.Arrow.Extended
import           Control.Lens

import qualified Hasura.Incremental               as Inc

import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.Run
import           Hasura.SQL.Types

-- | 'InvalidationKeys' used to apply requested 'CacheInvalidations'.
data InvalidationKeys = InvalidationKeys
  { _ikMetadata      :: !Inc.InvalidationKey
  , _ikRemoteSchemas :: !(HashMap RemoteSchemaName Inc.InvalidationKey)
  } deriving (Eq, Generic)
instance Inc.Cacheable InvalidationKeys
instance Inc.Select InvalidationKeys
$(makeLenses ''InvalidationKeys)

initialInvalidationKeys :: InvalidationKeys
initialInvalidationKeys = InvalidationKeys Inc.initialInvalidationKey mempty

invalidateKeys :: CacheInvalidations -> InvalidationKeys -> InvalidationKeys
invalidateKeys CacheInvalidations{..} InvalidationKeys{..} = InvalidationKeys
  { _ikMetadata = if ciMetadata then Inc.invalidate _ikMetadata else _ikMetadata
  , _ikRemoteSchemas = foldl' (flip invalidateRemoteSchema) _ikRemoteSchemas ciRemoteSchemas }
  where
    invalidateRemoteSchema = M.alter $ Just . maybe Inc.initialInvalidationKey Inc.invalidate

data BuildInputs
  = BuildInputs
  { _biReason          :: !BuildReason
  , _biCatalogMetadata :: !CatalogMetadata
  , _biInvalidationMap :: !InvalidationKeys
  } deriving (Eq)

-- | The direct output of 'buildSchemaCacheRule'. Contains most of the things necessary to build a
-- schema cache, but dependencies and inconsistent metadata objects are collected via a separate
-- 'MonadWriter' side channel.
data BuildOutputs
  = BuildOutputs
  { _boTables        :: !TableCache
  , _boActions       :: !ActionCache
  , _boFunctions     :: !FunctionCache
  , _boRemoteSchemas :: !(HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject))
  -- ^ We preserve the 'MetadataObject' from the original catalog metadata in the output so we can
  -- reuse it later if we need to mark the remote schema inconsistent during GraphQL schema
  -- generation (because of field conflicts).
  , _boAllowlist     :: !(HS.HashSet GQLQuery)
  , _boCustomTypes   :: !AnnotatedCustomTypes
  } deriving (Show, Eq)
$(makeLenses ''BuildOutputs)

data RebuildableSchemaCache m
  = RebuildableSchemaCache
  { lastBuiltSchemaCache :: !SchemaCache
  , _rscInvalidationMap :: !InvalidationKeys
  , _rscRebuild :: !(Inc.Rule (ReaderT BuildReason m) (CatalogMetadata, InvalidationKeys) SchemaCache)
  }
$(makeLenses ''RebuildableSchemaCache)

type CacheBuildM = ReaderT BuildReason Run
type CacheBuildA = WriterA (Seq CollectedInfo) (Inc.Rule CacheBuildM)

bindErrorA
  :: (ArrowChoice arr, ArrowKleisli m arr, ArrowError e arr, MonadError e m)
  => arr (m a) a
bindErrorA = liftEitherA <<< arrM \m -> (Right <$> m) `catchError` (pure . Left)
{-# INLINE bindErrorA #-}

withRecordDependencies
  :: (ArrowWriter (Seq CollectedInfo) arr)
  => WriterA (Seq SchemaDependency) arr (e, s) a
  -> arr (e, (MetadataObject, (SchemaObjId, s))) a
withRecordDependencies f = proc (e, (metadataObject, (schemaObjectId, s))) -> do
  (result, dependencies) <- runWriterA f -< (e, s)
  recordDependencies -< (metadataObject, schemaObjectId, toList dependencies)
  returnA -< result
{-# INLINABLE withRecordDependencies #-}

noDuplicates
  :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
  => (a -> MetadataObject)
  -> [a] `arr` Maybe a
noDuplicates mkMetadataObject = proc values -> case values of
  []      -> returnA -< Nothing
  [value] -> returnA -< Just value
  value:_ -> do
    let objectId = _moId $ mkMetadataObject value
        definitions = map (_moDefinition . mkMetadataObject) values
    tellA -< Seq.singleton $ CIInconsistency (DuplicateObjects objectId definitions)
    returnA -< Nothing
{-# INLINABLE noDuplicates #-}

-- | Processes a list of catalog metadata into a map of processed information, marking any duplicate
-- entries inconsistent.
buildInfoMap
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Eq k, Hashable k )
  => (a -> k)
  -> (a -> MetadataObject)
  -> (e, a) `arr` Maybe b
  -> (e, [a]) `arr` HashMap k b
buildInfoMap extractKey mkMetadataObject buildInfo = proc (e, infos) ->
      (M.groupOn extractKey infos >- returnA)
  >-> (| Inc.keyed (\_ duplicateInfos ->
               (duplicateInfos >- noDuplicates mkMetadataObject)
           >-> (| traverseA (\info -> (e, info) >- buildInfo) |)
           >-> (\info -> join info >- returnA)) |)
  >-> (\infoMap -> M.catMaybes infoMap >- returnA)
{-# INLINABLE buildInfoMap #-}

-- | Like 'buildInfo', but includes each processed info’s associated 'MetadataObject' in the result.
-- This is useful if the results will be further processed, and the 'MetadataObject' is still needed
-- to mark the object inconsistent.
buildInfoMapPreservingMetadata
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Eq k, Hashable k )
  => (a -> k)
  -> (a -> MetadataObject)
  -> (e, a) `arr` Maybe b
  -> (e, [a]) `arr` HashMap k (b, MetadataObject)
buildInfoMapPreservingMetadata extractKey mkMetadataObject buildInfo =
  buildInfoMap extractKey mkMetadataObject proc (e, info) ->
    ((e, info) >- buildInfo) >-> \result -> result <&> (, mkMetadataObject info) >- returnA
{-# INLINABLE buildInfoMapPreservingMetadata #-}

addTableContext :: QualifiedTable -> Text -> Text
addTableContext tableName e = "in table " <> tableName <<> ": " <> e
