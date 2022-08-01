{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types/functions shared between modules that implement "Hasura.RQL.DDL.Schema.Cache". Other
-- modules should not import this module directly.
module Hasura.RQL.DDL.Schema.Cache.Common
  ( ApolloFederationConfig (..),
    ApolloFederationVersion (..),
    BuildOutputs (..),
    CacheBuild,
    CacheBuildParams (CacheBuildParams),
    InvalidationKeys (..),
    ikMetadata,
    ikRemoteSchemas,
    ikSources,
    NonColumnTableInputs (..),
    RebuildableSchemaCache (RebuildableSchemaCache, lastBuiltSchemaCache),
    TableBuildInput (TableBuildInput, _tbiName),
    TablePermissionInputs (..),
    addTableContext,
    bindErrorA,
    boAllowlist,
    boApiLimits,
    boMetricsConfig,
    boTlsAllowlist,
    boActions,
    boCronTriggers,
    boCustomTypes,
    boEndpoints,
    boQueryCollections,
    boRemoteSchemas,
    boRoles,
    boSources,
    buildInfoMap,
    buildInfoMapPreservingMetadata,
    initialInvalidationKeys,
    invalidateKeys,
    mkTableInputs,
    runCacheBuild,
    runCacheBuildM,
    withRecordDependencies,
  )
where

import Control.Arrow.Extended
import Control.Arrow.Interpret
import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Unique
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.Incremental qualified as Inc
import Hasura.Prelude
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.SQL.Backend
import Hasura.Server.Types
import Hasura.Session
import Network.HTTP.Client.Manager (HasHttpManagerM (..))
import Network.HTTP.Client.Transformable qualified as HTTP

-- | 'InvalidationKeys' used to apply requested 'CacheInvalidations'.
data InvalidationKeys = InvalidationKeys
  { _ikMetadata :: Inc.InvalidationKey,
    _ikRemoteSchemas :: HashMap RemoteSchemaName Inc.InvalidationKey,
    _ikSources :: HashMap SourceName Inc.InvalidationKey
  }
  deriving (Show, Eq, Generic)

instance Inc.Cacheable InvalidationKeys

instance Inc.Select InvalidationKeys

$(makeLenses ''InvalidationKeys)

initialInvalidationKeys :: InvalidationKeys
initialInvalidationKeys = InvalidationKeys Inc.initialInvalidationKey mempty mempty

invalidateKeys :: CacheInvalidations -> InvalidationKeys -> InvalidationKeys
invalidateKeys CacheInvalidations {..} InvalidationKeys {..} =
  InvalidationKeys
    { _ikMetadata = if ciMetadata then Inc.invalidate _ikMetadata else _ikMetadata,
      _ikRemoteSchemas = foldl' (flip invalidate) _ikRemoteSchemas ciRemoteSchemas,
      _ikSources = foldl' (flip invalidate) _ikSources ciSources
    }
  where
    invalidate ::
      (Eq a, Hashable a) =>
      a ->
      HashMap a Inc.InvalidationKey ->
      HashMap a Inc.InvalidationKey
    invalidate = M.alter $ Just . maybe Inc.initialInvalidationKey Inc.invalidate

data TableBuildInput b = TableBuildInput
  { _tbiName :: TableName b,
    _tbiIsEnum :: Bool,
    _tbiConfiguration :: TableConfig b,
    _tbiApolloFederationConfig :: Maybe ApolloFederationConfig
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => NFData (TableBuildInput b)

instance (Backend b) => Inc.Cacheable (TableBuildInput b)

data NonColumnTableInputs b = NonColumnTableInputs
  { _nctiTable :: TableName b,
    _nctiObjectRelationships :: [ObjRelDef b],
    _nctiArrayRelationships :: [ArrRelDef b],
    _nctiComputedFields :: [ComputedFieldMetadata b],
    _nctiRemoteRelationships :: [RemoteRelationship]
  }
  deriving (Show, Eq, Generic)

data TablePermissionInputs b = TablePermissionInputs
  { _tpiTable :: TableName b,
    _tpiInsert :: [InsPermDef b],
    _tpiSelect :: [SelPermDef b],
    _tpiUpdate :: [UpdPermDef b],
    _tpiDelete :: [DelPermDef b]
  }
  deriving (Generic)

deriving instance (Backend b, Show (TableName b)) => Show (TablePermissionInputs b)

deriving instance (Backend b, Eq (TableName b)) => Eq (TablePermissionInputs b)

instance (Backend b) => Inc.Cacheable (TablePermissionInputs b)

mkTableInputs ::
  TableMetadata b -> (TableBuildInput b, NonColumnTableInputs b, TablePermissionInputs b)
mkTableInputs TableMetadata {..} =
  (buildInput, nonColumns, permissions)
  where
    buildInput = TableBuildInput _tmTable _tmIsEnum _tmConfiguration _tmApolloFederationConfig
    nonColumns =
      NonColumnTableInputs
        _tmTable
        (OMap.elems _tmObjectRelationships)
        (OMap.elems _tmArrayRelationships)
        (OMap.elems _tmComputedFields)
        (OMap.elems _tmRemoteRelationships)
    permissions =
      TablePermissionInputs
        _tmTable
        (OMap.elems _tmInsertPermissions)
        (OMap.elems _tmSelectPermissions)
        (OMap.elems _tmUpdatePermissions)
        (OMap.elems _tmDeletePermissions)

-- | The direct output of 'buildSchemaCacheRule'. Contains most of the things necessary to build a
-- schema cache, but dependencies and inconsistent metadata objects are collected via a separate
-- 'MonadWriter' side channel.
data BuildOutputs = BuildOutputs
  { _boSources :: SourceCache,
    _boActions :: ActionCache,
    -- | We preserve the 'MetadataObject' from the original catalog metadata in the output so we can
    -- reuse it later if we need to mark the remote schema inconsistent during GraphQL schema
    -- generation (because of field conflicts).
    _boRemoteSchemas :: HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject),
    _boAllowlist :: InlinedAllowlist,
    _boCustomTypes :: AnnotatedCustomTypes,
    _boCronTriggers :: M.HashMap TriggerName CronTriggerInfo,
    _boEndpoints :: M.HashMap EndpointName (EndpointMetadata GQLQueryWithText),
    _boApiLimits :: ApiLimit,
    _boMetricsConfig :: MetricsConfig,
    _boRoles :: HashMap RoleName Role,
    _boTlsAllowlist :: [TlsAllow],
    _boQueryCollections :: QueryCollections
  }

$(makeLenses ''BuildOutputs)

-- | Parameters required for schema cache build
data CacheBuildParams = CacheBuildParams
  { _cbpManager :: HTTP.Manager,
    _cbpPGSourceResolver :: SourceResolver ('Postgres 'Vanilla),
    _cbpMSSQLSourceResolver :: SourceResolver 'MSSQL,
    _cbpServerConfigCtx :: ServerConfigCtx
  }

-- | The monad in which @'RebuildableSchemaCache' is being run
newtype CacheBuild a = CacheBuild (ReaderT CacheBuildParams (ExceptT QErr IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError QErr,
      MonadReader CacheBuildParams,
      MonadIO,
      MonadBase IO,
      MonadBaseControl IO,
      MonadUnique
    )

instance HasHttpManagerM CacheBuild where
  askHttpManager = asks _cbpManager

instance HasServerConfigCtx CacheBuild where
  askServerConfigCtx = asks _cbpServerConfigCtx

instance MonadResolveSource CacheBuild where
  getPGSourceResolver = asks _cbpPGSourceResolver
  getMSSQLSourceResolver = asks _cbpMSSQLSourceResolver

runCacheBuild ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  CacheBuildParams ->
  CacheBuild a ->
  m a
runCacheBuild params (CacheBuild m) = do
  liftEitherM $ liftIO $ runExceptT (runReaderT m params)

runCacheBuildM ::
  ( MonadIO m,
    MonadError QErr m,
    HasHttpManagerM m,
    HasServerConfigCtx m,
    MonadResolveSource m
  ) =>
  CacheBuild a ->
  m a
runCacheBuildM m = do
  params <-
    CacheBuildParams
      <$> askHttpManager
      <*> getPGSourceResolver
      <*> getMSSQLSourceResolver
      <*> askServerConfigCtx
  runCacheBuild params m

data RebuildableSchemaCache = RebuildableSchemaCache
  { lastBuiltSchemaCache :: SchemaCache,
    _rscInvalidationMap :: InvalidationKeys,
    _rscRebuild :: Inc.Rule (ReaderT BuildReason CacheBuild) (Metadata, InvalidationKeys) SchemaCache
  }

bindErrorA ::
  (ArrowChoice arr, ArrowKleisli m arr, ArrowError e arr, MonadError e m) =>
  arr (m a) a
bindErrorA = liftEitherA <<< arrM \m -> (Right <$> m) `catchError` (pure . Left)
{-# INLINE bindErrorA #-}

withRecordDependencies ::
  (ArrowWriter (Seq CollectedInfo) arr) =>
  WriterA (Seq SchemaDependency) arr (e, s) a ->
  arr (e, (MetadataObject, (SchemaObjId, s))) a
withRecordDependencies f = proc (e, (metadataObject, (schemaObjectId, s))) -> do
  (result, dependencies) <- runWriterA f -< (e, s)
  recordDependencies -< (metadataObject, schemaObjectId, toList dependencies)
  returnA -< result
{-# INLINEABLE withRecordDependencies #-}

noDuplicates ::
  (MonadWriter (Seq CollectedInfo) m) =>
  (a -> MetadataObject) ->
  [a] ->
  m (Maybe a)
noDuplicates mkMetadataObject = \case
  [] -> pure Nothing
  [value] -> pure $ Just value
  values@(value : _) -> do
    let objectId = _moId $ mkMetadataObject value
        definitions = map (_moDefinition . mkMetadataObject) values
    tell $ Seq.singleton $ CIInconsistency (DuplicateObjects objectId definitions)
    return Nothing

-- | Processes a list of catalog metadata into a map of processed information, marking any duplicate
-- entries inconsistent.
buildInfoMap ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectedInfo) arr,
    Eq k,
    Hashable k
  ) =>
  (a -> k) ->
  (a -> MetadataObject) ->
  (e, a) `arr` Maybe b ->
  (e, [a]) `arr` HashMap k b
buildInfoMap extractKey mkMetadataObject buildInfo = proc (e, infos) ->
  (M.groupOn extractKey infos >- returnA)
    >-> (|
          Inc.keyed
            ( \_ duplicateInfos ->
                (noDuplicates mkMetadataObject duplicateInfos >- interpretWriter)
                  >-> (| traverseA (\info -> (e, info) >- buildInfo) |)
                  >-> (\info -> join info >- returnA)
            )
        |)
    >-> (\infoMap -> catMaybes infoMap >- returnA)
{-# INLINEABLE buildInfoMap #-}

-- | Like 'buildInfo', but includes each processed info’s associated 'MetadataObject' in the result.
-- This is useful if the results will be further processed, and the 'MetadataObject' is still needed
-- to mark the object inconsistent.
buildInfoMapPreservingMetadata ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectedInfo) arr,
    Eq k,
    Hashable k
  ) =>
  (a -> k) ->
  (a -> MetadataObject) ->
  (e, a) `arr` Maybe b ->
  (e, [a]) `arr` HashMap k (b, MetadataObject)
buildInfoMapPreservingMetadata extractKey mkMetadataObject buildInfo =
  buildInfoMap extractKey mkMetadataObject proc (e, info) ->
    ((e, info) >- buildInfo) >-> \result -> result <&> (,mkMetadataObject info) >- returnA
{-# INLINEABLE buildInfoMapPreservingMetadata #-}

addTableContext :: (Backend b) => TableName b -> Text -> Text
addTableContext tableName e = "in table " <> tableName <<> ": " <> e
