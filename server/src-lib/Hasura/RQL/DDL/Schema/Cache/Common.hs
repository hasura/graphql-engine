{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types/functions shared between modules that implement "Hasura.RQL.DDL.Schema.Cache". Other
-- modules should not import this module directly.
module Hasura.RQL.DDL.Schema.Cache.Common
  ( ApolloFederationConfig (..),
    ApolloFederationVersion (..),
    BackendInvalidationKeysWrapper (..),
    BackendIntrospection (..),
    BuildOutputs (..),
    CacheBuild,
    CacheBuildParams (CacheBuildParams),
    InvalidationKeys (..),
    StoredIntrospection (..),
    ikMetadata,
    ikRemoteSchemas,
    ikSources,
    ikBackends,
    NonColumnTableInputs (..),
    RebuildableSchemaCache (RebuildableSchemaCache, lastBuiltSchemaCache),
    TableBuildInput (TableBuildInput, _tbiName),
    TablePermissionInputs (..),
    addTableContext,
    bindErrorA,
    boActions,
    boCustomTypes,
    boBackendCache,
    boRemoteSchemas,
    boRoles,
    boSources,
    buildInfoMap,
    buildInfoMapM,
    buildInfoMapPreservingMetadata,
    buildInfoMapPreservingMetadataM,
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
import Data.Aeson.Extended
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Incremental qualified as Inc
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.RemoteSchema.Metadata
import Hasura.SQL.AnyBackend
import Hasura.SQL.Backend
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Server.Types
import Hasura.Session
import Network.HTTP.Client.Manager (HasHttpManagerM (..))
import Network.HTTP.Client.Transformable qualified as HTTP

newtype BackendInvalidationKeysWrapper (b :: BackendType) = BackendInvalidationKeysWrapper
  { unBackendInvalidationKeysWrapper :: BackendInvalidationKeys b
  }

deriving newtype instance Eq (BackendInvalidationKeys b) => Eq (BackendInvalidationKeysWrapper b)

deriving newtype instance Ord (BackendInvalidationKeys b) => Ord (BackendInvalidationKeysWrapper b)

deriving newtype instance Show (BackendInvalidationKeys b) => Show (BackendInvalidationKeysWrapper b)

deriving newtype instance Semigroup (BackendInvalidationKeys b) => Semigroup (BackendInvalidationKeysWrapper b)

deriving newtype instance Monoid (BackendInvalidationKeys b) => Monoid (BackendInvalidationKeysWrapper b)

instance Inc.Select (BackendInvalidationKeysWrapper b)

-- | 'InvalidationKeys' used to apply requested 'CacheInvalidations'.
data InvalidationKeys = InvalidationKeys
  { _ikMetadata :: Inc.InvalidationKey,
    _ikRemoteSchemas :: HashMap RemoteSchemaName Inc.InvalidationKey,
    _ikSources :: HashMap SourceName Inc.InvalidationKey,
    _ikBackends :: BackendMap BackendInvalidationKeysWrapper
  }
  deriving (Show, Eq, Generic)

instance Inc.Select InvalidationKeys

$(makeLenses ''InvalidationKeys)

initialInvalidationKeys :: InvalidationKeys
initialInvalidationKeys = InvalidationKeys Inc.initialInvalidationKey mempty mempty mempty

invalidateKeys :: CacheInvalidations -> InvalidationKeys -> InvalidationKeys
invalidateKeys CacheInvalidations {..} InvalidationKeys {..} =
  InvalidationKeys
    { _ikMetadata = if ciMetadata then Inc.invalidate _ikMetadata else _ikMetadata,
      _ikRemoteSchemas = foldl' (flip invalidate) _ikRemoteSchemas ciRemoteSchemas,
      _ikSources = foldl' (flip invalidate) _ikSources ciSources,
      _ikBackends = BackendMap.modify @'DataConnector invalidateDataConnectors _ikBackends
    }
  where
    invalidate ::
      Hashable a =>
      a ->
      HashMap a Inc.InvalidationKey ->
      HashMap a Inc.InvalidationKey
    invalidate = M.alter $ Just . maybe Inc.initialInvalidationKey Inc.invalidate

    invalidateDataConnectors :: BackendInvalidationKeysWrapper 'DataConnector -> BackendInvalidationKeysWrapper 'DataConnector
    invalidateDataConnectors (BackendInvalidationKeysWrapper invalidationKeys) =
      BackendInvalidationKeysWrapper $ foldl' (flip invalidate) invalidationKeys ciDataConnectors

data BackendIntrospection (b :: BackendType) = BackendIntrospection
  { biMetadata :: DBObjectsIntrospection b,
    biEnumValues :: HashMap (TableName b) EnumValues
  }
  deriving stock (Generic)

instance Backend b => FromJSON (BackendIntrospection b) where
  parseJSON = withObject "BackendIntrospection" \o -> do
    metadata <- o .: "metadata"
    enumValues <- o .: "enum_values"
    pure $ BackendIntrospection metadata (M.fromList enumValues)

deriving stock instance BackendMetadata b => Eq (BackendIntrospection b)

data StoredIntrospection = StoredIntrospection
  { siBackendIntrospection :: HashMap SourceName (AnyBackend BackendIntrospection),
    -- We'd prefer to pass the results of introspecting remote GraphQL schemas
    -- as a structured Haskell type, rather than the opaque `EncJSON`.  What
    -- makes this complicated is that, in the `introspect_remote_schema` API, we
    -- need to return the result of the specific introspection query stored in
    -- `server/src-rsr/introspection.json`.  That has a very specific format
    -- (see e.g. `fragment TypeRef`).  Additionally, it requires us to return
    -- introspection results for directives, which so far we have avoided
    -- parsing entirely (for better or for worse).
    --
    -- In the future, perhaps we can change the implementation of
    -- `introspect_remote_schema` to be backed by Stored Introspection directly.
    -- Then we could pass around highly structured data internally here, but
    -- also return the original raw introspection to the user on request.  But
    -- this approach would require a wholesale commitment to Stored
    -- Introspection.
    siRemotes :: HashMap RemoteSchemaName EncJSON
  }

instance Eq StoredIntrospection where
  -- compare introspected remotes as serialized values, not as JSON value equality
  StoredIntrospection bi1 r1 == StoredIntrospection bi2 r2 = bi1 == bi2 && (encJToBS <$> r1) == (encJToBS <$> r2)

instance FromJSON StoredIntrospection where
  parseJSON = withObject "StoredIntrospection" \o -> do
    -- Use of `parseJSONKeyValue` here means that the backend type is specified as a key
    backendIntrospection <- traverse parseJSONKeyValue =<< o .: "backend_introspection"
    remotes <- o .: "remotes"
    pure $ StoredIntrospection backendIntrospection remotes

data TableBuildInput b = TableBuildInput
  { _tbiName :: TableName b,
    _tbiIsEnum :: Bool,
    _tbiConfiguration :: TableConfig b,
    _tbiApolloFederationConfig :: Maybe ApolloFederationConfig
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => NFData (TableBuildInput b)

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

deriving instance (Backend b) => Show (TablePermissionInputs b)

deriving instance (Backend b) => Eq (TablePermissionInputs b)

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
--
-- See also Note [Avoiding GraphQL schema rebuilds when changing irrelevant Metadata]
data BuildOutputs = BuildOutputs
  { _boSources :: SourceCache,
    _boActions :: ActionCache,
    -- | We preserve the 'MetadataObject' from the original catalog metadata in the output so we can
    -- reuse it later if we need to mark the remote schema inconsistent during GraphQL schema
    -- generation (because of field conflicts).
    _boRemoteSchemas :: HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject),
    _boCustomTypes :: AnnotatedCustomTypes,
    _boRoles :: HashMap RoleName Role,
    _boBackendCache :: BackendCache
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
  deriving newtype
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
    _rscRebuild :: Inc.Rule (ReaderT BuildReason CacheBuild) (Metadata, InvalidationKeys, Maybe StoredIntrospection) SchemaCache
  }

bindErrorA ::
  (ArrowChoice arr, ArrowKleisli m arr, ArrowError e arr, MonadError e m) =>
  arr (m a) a
bindErrorA = liftEitherA <<< arrM \m -> (Right <$> m) `catchError` (pure . Left)
{-# INLINE bindErrorA #-}

withRecordDependencies ::
  (ArrowWriter (Seq (Either im MetadataDependency)) arr) =>
  WriterA (Seq SchemaDependency) arr (e, s) a ->
  arr (e, (MetadataObject, (SchemaObjId, s))) a
withRecordDependencies f = proc (e, (metadataObject, (schemaObjectId, s))) -> do
  (result, dependencies) <- runWriterA f -< (e, s)
  recordDependencies -< (metadataObject, schemaObjectId, dependencies)
  returnA -< result
{-# INLINEABLE withRecordDependencies #-}

noDuplicates ::
  (MonadWriter (Seq (Either InconsistentMetadata md)) m) =>
  (a -> MetadataObject) ->
  [a] ->
  m (Maybe a)
noDuplicates mkMetadataObject = \case
  [] -> pure Nothing
  [value] -> pure $ Just value
  values@(value : _) -> do
    let objectId = _moId $ mkMetadataObject value
        definitions = map (_moDefinition . mkMetadataObject) values
    tell $ Seq.singleton $ Left (DuplicateObjects objectId definitions)
    return Nothing

-- | Processes a list of catalog metadata into a map of processed information, marking any duplicate
-- entries inconsistent.
buildInfoMap ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq (Either InconsistentMetadata md)) arr,
    Hashable k
  ) =>
  (a -> k) ->
  (a -> MetadataObject) ->
  (e, a) `arr` Maybe b ->
  (e, [a]) `arr` HashMap k b
buildInfoMap extractKey mkMetadataObject buildInfo = proc (e, infos) -> do
  let groupedInfos = M.groupOn extractKey infos
  infoMapMaybes <-
    (|
      Inc.keyed
        ( \_ duplicateInfos -> do
            infoMaybe <- interpretWriter -< noDuplicates mkMetadataObject duplicateInfos
            case infoMaybe of
              Nothing -> returnA -< Nothing
              Just info -> buildInfo -< (e, info)
        )
      |) groupedInfos
  returnA -< catMaybes infoMapMaybes
{-# INLINEABLE buildInfoMap #-}

buildInfoMapM ::
  ( MonadWriter (Seq (Either InconsistentMetadata md)) m,
    Hashable k
  ) =>
  (a -> k) ->
  (a -> MetadataObject) ->
  (a -> m (Maybe b)) ->
  [a] ->
  m (HashMap k b)
buildInfoMapM extractKey mkMetadataObject buildInfo infos = do
  let groupedInfos = M.groupOn extractKey infos
  infoMapMaybes <- for groupedInfos \duplicateInfos -> do
    infoMaybe <- noDuplicates mkMetadataObject duplicateInfos
    case infoMaybe of
      Nothing -> pure Nothing
      Just info -> do
        buildInfo info
  pure $ catMaybes infoMapMaybes

-- | Like 'buildInfoMap', but includes each processed info’s associated 'MetadataObject' in the result.
-- This is useful if the results will be further processed, and the 'MetadataObject' is still needed
-- to mark the object inconsistent.
buildInfoMapPreservingMetadata ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq (Either InconsistentMetadata md)) arr,
    Hashable k
  ) =>
  (a -> k) ->
  (a -> MetadataObject) ->
  (e, a) `arr` Maybe b ->
  (e, [a]) `arr` HashMap k (b, MetadataObject)
buildInfoMapPreservingMetadata extractKey mkMetadataObject buildInfo =
  buildInfoMap extractKey mkMetadataObject buildInfoPreserving
  where
    buildInfoPreserving = proc (e, info) -> do
      result <- buildInfo -< (e, info)
      returnA -< result <&> (,mkMetadataObject info)
{-# INLINEABLE buildInfoMapPreservingMetadata #-}

buildInfoMapPreservingMetadataM ::
  ( MonadWriter (Seq (Either InconsistentMetadata md)) m,
    Hashable k
  ) =>
  (a -> k) ->
  (a -> MetadataObject) ->
  (a -> m (Maybe b)) ->
  [a] ->
  m (HashMap k (b, MetadataObject))
buildInfoMapPreservingMetadataM extractKey mkMetadataObject buildInfo =
  buildInfoMapM extractKey mkMetadataObject buildInfoPreserving
  where
    buildInfoPreserving info = do
      result <- buildInfo info
      pure $ result <&> (,mkMetadataObject info)

addTableContext :: (Backend b) => TableName b -> Text -> Text
addTableContext tableName e = "in table " <> tableName <<> ": " <> e
