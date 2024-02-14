{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Metadata
  ( Metadata (..),
    MetadataM (..),
    MetadataModifier (..),
    MetadataNoSources (..),
    MetadataVersion (..),
    MetadataDefaults (..),
    currentMetadataVersion,
    dropComputedFieldInMetadata,
    dropEventTriggerInMetadata,
    dropFunctionInMetadata,
    dropPermissionInMetadata,
    dropLogicalModelPermissionInMetadata,
    dropInlineLogicalModelPermissionInMetadata,
    dropRelationshipInMetadata,
    dropNativeQueryRelationshipInMetadata,
    dropRemoteRelationshipInMetadata,
    dropTableInMetadata,
    dropRemoteSchemaInMetadata,
    dropRemoteSchemaPermissionInMetadata,
    dropRemoteSchemaRemoteRelationshipInMetadata,
    emptyMetadata,
    emptyMetadataDefaults,
    functionMetadataSetter,
    logicalModelMetadataSetter,
    nativeQueryMetadataSetter,
    storedProcedureMetadataSetter,
    metaActions,
    metaAllowlist,
    metaApiLimits,
    metaBackendConfigs,
    metaCronTriggers,
    metaCustomTypes,
    metaInheritedRoles,
    metaMetricsConfig,
    metaNetwork,
    metaOpenTelemetryConfig,
    metaQueryCollections,
    metaRemoteSchemas,
    metaRestEndpoints,
    metaSetGraphqlIntrospectionOptions,
    metaSources,
    metadataToDTO,
    metadataToOrdJSON,
    overrideMetadataDefaults,
    tableMetadataSetter,
    module Hasura.RQL.Types.Metadata.Common,
  )
where

import Control.Lens hiding (set, (.=))
import Data.Aeson.Extended (FromJSONWithContext (..), mapWithJSONPath)
import Data.Aeson.KeyMap (singleton)
import Data.Aeson.Ordered qualified as AO
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.Monoid (Dual (..), Endo (..))
import Hasura.Function.Cache
import Hasura.Function.Metadata (FunctionMetadata (..))
import Hasura.Incremental qualified as Inc
import Hasura.LogicalModel.Lenses (lmmSelectPermissions)
import Hasura.LogicalModel.Metadata (LogicalModelMetadata, LogicalModelName)
import Hasura.LogicalModelResolver.Lenses (ilmmSelectPermissions)
import Hasura.LogicalModelResolver.Metadata (InlineLogicalModelMetadata (..))
import Hasura.Metadata.DTO.MetadataV3 (MetadataV3 (..))
import Hasura.NativeQuery.Lenses (nqmArrayRelationships)
import Hasura.NativeQuery.Metadata (NativeQueryMetadata, NativeQueryName)
import Hasura.Prelude
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata.Common
import Hasura.RQL.Types.Metadata.Serialization
import Hasura.RQL.Types.OpenTelemetry
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RemoteSchema.Metadata
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.StoredProcedure.Metadata (StoredProcedureMetadata)
import Hasura.Table.Metadata
  ( TableMetadata (..),
    tmArrayRelationships,
    tmComputedFields,
    tmDeletePermissions,
    tmEventTriggers,
    tmInsertPermissions,
    tmObjectRelationships,
    tmRemoteRelationships,
    tmSelectPermissions,
    tmUpdatePermissions,
  )
import Hasura.Tracing (TraceT)
import Language.GraphQL.Draft.Syntax qualified as G
import Network.Types.Extended

-- | Versioning the @'Metadata' JSON structure to track backwards incompatible changes.
-- This value is included in the metadata JSON object at top level 'version' key.
-- Always metadata is emitted in the latest version via export metadata API (@'runExportMetadata' handler).
-- Adding a new value constructor to @'MetadataVersion' type bumps the metadata version.
--
-- NOTE: When metadata version is bumped:
-- 1. The Hasura CLI and Console actively use export metadata API to read metadata.
--    Hence, it is necessary to update CLI and Console to read latest metadata.
--    All changes SHOULD be released hand in hand (preferebly in one pull request)
-- 2. There might be other third party services (developed by Hasura users) which use
--    the export metadata API. Apart from changelog, we need to establish the metadata
--    version update by bumping up the minor version of the GraphQL Engine.
data MetadataVersion
  = MVVersion1
  | MVVersion2
  | MVVersion3
  deriving (Show, Eq, Generic)

instance ToJSON MetadataVersion where
  toJSON = \case
    MVVersion1 -> toJSON @Int 1
    MVVersion2 -> toJSON @Int 2
    MVVersion3 -> toJSON @Int 3

instance FromJSON MetadataVersion where
  parseJSON v = do
    version :: Int <- parseJSON v
    case version of
      1 -> pure MVVersion1
      2 -> pure MVVersion2
      3 -> pure MVVersion3
      i -> fail $ "expected 1, 2 or 3, encountered " ++ show i

currentMetadataVersion :: MetadataVersion
currentMetadataVersion = MVVersion3

-- | A complete GraphQL Engine metadata representation to be stored,
-- exported/replaced via metadata queries.
data Metadata = Metadata
  { _metaSources :: Sources,
    _metaRemoteSchemas :: RemoteSchemas,
    _metaQueryCollections :: QueryCollections,
    _metaAllowlist :: MetadataAllowlist,
    _metaCustomTypes :: CustomTypes,
    _metaActions :: Actions,
    _metaCronTriggers :: CronTriggers,
    _metaRestEndpoints :: Endpoints,
    _metaApiLimits :: ApiLimit,
    _metaMetricsConfig :: MetricsConfig,
    _metaInheritedRoles :: InheritedRoles,
    _metaSetGraphqlIntrospectionOptions :: SetGraphqlIntrospectionOptions,
    _metaNetwork :: Network,
    _metaBackendConfigs :: BackendMap BackendConfigWrapper,
    _metaOpenTelemetryConfig :: OpenTelemetryConfig
  }
  deriving (Show, Eq, Generic)

instance Inc.Select Metadata

$(makeLenses ''Metadata)

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    when (version /= MVVersion3)
      $ fail
      $ "unexpected metadata version from storage: "
      <> show version
    rawSources <- o .: "sources"
    backendConfigs <- o .:? "backend_configs" .!= mempty
    sources <- oMapFromL getSourceName <$> mapWithJSONPath parseSourceMetadata rawSources <?> Key "sources"
    endpoints <- oMapFromL _ceName <$> o .:? "rest_endpoints" .!= []
    network <- o .:? "network" .!= emptyNetwork
    openTelemetry <- o .:? "opentelemetry" .!= emptyOpenTelemetryConfig
    ( remoteSchemas,
      queryCollections,
      allowlist,
      customTypes,
      actions,
      cronTriggers,
      apiLimits,
      metricsConfig,
      inheritedRoles,
      disabledSchemaIntrospectionRoles
      ) <-
      parseNonSourcesMetadata o
    pure
      $ Metadata
        sources
        remoteSchemas
        queryCollections
        allowlist
        customTypes
        actions
        cronTriggers
        endpoints
        apiLimits
        metricsConfig
        inheritedRoles
        disabledSchemaIntrospectionRoles
        network
        backendConfigs
        openTelemetry
    where
      parseSourceMetadata :: Value -> Parser BackendSourceMetadata
      parseSourceMetadata = withObject "SourceMetadata" \o -> do
        backendSourceKind <- explicitParseFieldMaybe AB.parseBackendSourceKindFromJSON o "kind" .!= AB.mkAnyBackend PostgresVanillaKind
        AB.dispatchAnyBackend @Backend
          backendSourceKind
          ( \(kind :: BackendSourceKind b) ->
              BackendSourceMetadata . AB.mkAnyBackend @b <$> parseJSONWithContext kind (Object o)
          )

emptyMetadata :: Metadata
emptyMetadata =
  Metadata
    { _metaSources = mempty,
      _metaRemoteSchemas = mempty,
      _metaQueryCollections = mempty,
      _metaAllowlist = mempty,
      _metaActions = mempty,
      _metaCronTriggers = mempty,
      _metaRestEndpoints = mempty,
      _metaInheritedRoles = mempty,
      _metaSetGraphqlIntrospectionOptions = mempty,
      _metaCustomTypes = emptyCustomTypes,
      _metaApiLimits = emptyApiLimit,
      _metaMetricsConfig = emptyMetricsConfig,
      _metaNetwork = emptyNetwork,
      _metaBackendConfigs = mempty,
      _metaOpenTelemetryConfig = emptyOpenTelemetryConfig
    }

-- | This type serves to allow Metadata arguments to be distinguished
newtype MetadataDefaults = MetadataDefaults Metadata
  deriving (Eq, Show)

-- | The metadata instance first defaults the version and sources fields, then defers to the Metadata FromJSON instance
instance FromJSON MetadataDefaults where
  parseJSON o = MetadataDefaults <$> (parseJSON =<< defaultVersionAndSources o)
    where
      defaultVersionAndSources = \case
        (Object o') -> pure (Object (o' <> versionSingleton <> sourcesSingleton))
        _ -> fail "Was expecting an Object for Metadata"
        where
          versionSingleton = singleton "version" (Number 3)
          sourcesSingleton = singleton "sources" (Array mempty)

emptyMetadataDefaults :: MetadataDefaults
emptyMetadataDefaults = MetadataDefaults emptyMetadata

-- | This acts like a Semigroup instance for Metadata, favouring the non-default Metadata
overrideMetadataDefaults :: Metadata -> MetadataDefaults -> Metadata
overrideMetadataDefaults md (MetadataDefaults defs) =
  Metadata
    { _metaSources = (_metaSources md) <> (_metaSources defs),
      _metaRemoteSchemas = (_metaRemoteSchemas md) <> (_metaRemoteSchemas defs),
      _metaQueryCollections = (_metaQueryCollections md) <> (_metaQueryCollections defs),
      _metaAllowlist = (_metaAllowlist md) <> (_metaAllowlist defs),
      _metaActions = (_metaActions md) <> (_metaActions defs),
      _metaCronTriggers = (_metaCronTriggers md) <> (_metaCronTriggers defs),
      _metaRestEndpoints = (_metaRestEndpoints md) <> (_metaRestEndpoints defs),
      _metaInheritedRoles = (_metaInheritedRoles md) <> (_metaInheritedRoles defs),
      _metaSetGraphqlIntrospectionOptions = (_metaSetGraphqlIntrospectionOptions md) <> (_metaSetGraphqlIntrospectionOptions defs),
      _metaCustomTypes = (_metaCustomTypes md) `overrideCustomTypesDefaults` (_metaCustomTypes defs),
      _metaApiLimits = (_metaApiLimits md) `overrideApiLimitsDefaults` (_metaApiLimits defs),
      _metaMetricsConfig = (_metaMetricsConfig md) `overrideMetricsConfigDefaults` (_metaMetricsConfig defs),
      _metaNetwork = (_metaNetwork md) `overrideNetworkDefaults` (_metaNetwork defs),
      _metaBackendConfigs = _metaBackendConfigs md `BackendMap.overridesDeeply` _metaBackendConfigs defs,
      _metaOpenTelemetryConfig = _metaOpenTelemetryConfig md -- no merge strategy implemented
    }
  where
    overrideCustomTypesDefaults (CustomTypes a1 a2 a3 a4) (CustomTypes b1 b2 b3 b4) = CustomTypes (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)
    overrideApiLimitsDefaults (ApiLimit a1 a2 a3 a4 a5 a6) (ApiLimit b1 b2 b3 b4 b5 b6) = ApiLimit (a1 <|> b1) (a2 <|> b2) (a3 <|> b3) (a4 <|> b4) (a5 <|> b5) (a6 || b6)
    overrideMetricsConfigDefaults (MetricsConfig a1 a2) (MetricsConfig b1 b2) = MetricsConfig (a1 || b1) (a2 || b2)
    overrideNetworkDefaults (Network a1) (Network b1) = Network (a1 <> b1)

tableMetadataSetter ::
  (Backend b) =>
  SourceName ->
  TableName b ->
  ASetter' Metadata (TableMetadata b)
tableMetadataSetter source table =
  metaSources . ix source . toSourceMetadata . smTables . ix table

-- | A lens setter for the metadata of a specific function as identified by
--   the source name and function name.
functionMetadataSetter ::
  (Backend b) =>
  SourceName ->
  FunctionName b ->
  ASetter' Metadata (FunctionMetadata b)
functionMetadataSetter source function =
  metaSources . ix source . toSourceMetadata . smFunctions . ix function

-- | A lens setter for the metadata of a logical model as identified by the
-- source name and root field name.
logicalModelMetadataSetter ::
  (Backend b) =>
  SourceName ->
  LogicalModelName ->
  ASetter' Metadata (LogicalModelMetadata b)
logicalModelMetadataSetter source name =
  metaSources . ix source . toSourceMetadata . smLogicalModels . ix name

-- | A lens setter for the metadata of a native query as identified by the
-- source name and root field name.
nativeQueryMetadataSetter ::
  (Backend b) =>
  SourceName ->
  NativeQueryName ->
  ASetter' Metadata (NativeQueryMetadata b)
nativeQueryMetadataSetter source nativeQueryName =
  metaSources . ix source . toSourceMetadata . smNativeQueries . ix nativeQueryName

-- | A lens setter for the metadata of a stored procedure as identified by the
-- source name and root field name.
storedProcedureMetadataSetter ::
  (Backend b) =>
  SourceName ->
  FunctionName b ->
  ASetter' Metadata (StoredProcedureMetadata b)
storedProcedureMetadataSetter source storedProcedureName =
  metaSources . ix source . toSourceMetadata . smStoredProcedures . ix storedProcedureName

-- | A simple monad class which enables fetching and setting @'Metadata'
-- in the state.
class (Monad m) => MetadataM m where
  getMetadata :: m Metadata
  putMetadata :: Metadata -> m ()

instance (MetadataM m) => MetadataM (ReaderT r m) where
  getMetadata = lift getMetadata
  putMetadata = lift . putMetadata

instance (MetadataM m) => MetadataM (StateT r m) where
  getMetadata = lift getMetadata
  putMetadata = lift . putMetadata

instance (MetadataM m) => MetadataM (TraceT m) where
  getMetadata = lift getMetadata
  putMetadata = lift . putMetadata

data MetadataNoSources = MetadataNoSources
  { _mnsTables :: Tables ('Postgres 'Vanilla),
    _mnsFunctions :: Functions ('Postgres 'Vanilla),
    _mnsRemoteSchemas :: RemoteSchemas,
    _mnsQueryCollections :: QueryCollections,
    _mnsAllowlist :: MetadataAllowlist,
    _mnsCustomTypes :: CustomTypes,
    _mnsActions :: Actions,
    _mnsCronTriggers :: CronTriggers
  }
  deriving stock (Eq, Generic)

instance ToJSON MetadataNoSources where
  toJSON = genericToJSON hasuraJSON

instance FromJSON MetadataNoSources where
  parseJSON = withObject "MetadataNoSources" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    (tables, functions) <-
      case version of
        MVVersion1 -> do
          tables <- oMapFromL _tmTable <$> o .: "tables"
          functionList <- o .:? "functions" .!= []
          let functions = InsOrdHashMap.fromList
                $ flip map functionList
                $ \function -> (function, FunctionMetadata function emptyFunctionConfig mempty Nothing)
          pure (tables, functions)
        MVVersion2 -> do
          tables <- oMapFromL _tmTable <$> o .: "tables"
          functions <- oMapFromL _fmFunction <$> o .:? "functions" .!= []
          pure (tables, functions)
        MVVersion3 -> fail "unexpected version for metadata without sources: 3"
    ( remoteSchemas,
      queryCollections,
      allowlist,
      customTypes,
      actions,
      cronTriggers,
      _,
      _,
      _,
      _
      ) <-
      parseNonSourcesMetadata o
    pure
      $ MetadataNoSources
        tables
        functions
        remoteSchemas
        queryCollections
        allowlist
        customTypes
        actions
        cronTriggers

newtype MetadataModifier = MetadataModifier {runMetadataModifier :: Metadata -> Metadata}
  deriving (Semigroup, Monoid) via (Dual (Endo Metadata))

dropTableInMetadata ::
  forall b. (Backend b) => SourceName -> TableName b -> MetadataModifier
dropTableInMetadata source table =
  MetadataModifier $ metaSources . ix source . (toSourceMetadata @b) . smTables %~ InsOrdHashMap.delete table

dropRelationshipInMetadata ::
  RelName -> TableMetadata b -> TableMetadata b
dropRelationshipInMetadata relName =
  -- Since the name of a relationship is unique in a table, the relationship
  -- with given name may present in either array or object relationships but
  -- not in both.
  (tmObjectRelationships %~ InsOrdHashMap.delete relName)
    . (tmArrayRelationships %~ InsOrdHashMap.delete relName)

dropNativeQueryRelationshipInMetadata :: RelName -> NativeQueryMetadata b -> NativeQueryMetadata b
dropNativeQueryRelationshipInMetadata relName =
  nqmArrayRelationships %~ InsOrdHashMap.delete relName

dropPermissionInMetadata ::
  RoleName -> PermType -> TableMetadata b -> TableMetadata b
dropPermissionInMetadata rn = \case
  PTInsert -> tmInsertPermissions %~ InsOrdHashMap.delete rn
  PTSelect -> tmSelectPermissions %~ InsOrdHashMap.delete rn
  PTDelete -> tmDeletePermissions %~ InsOrdHashMap.delete rn
  PTUpdate -> tmUpdatePermissions %~ InsOrdHashMap.delete rn

dropLogicalModelPermissionInMetadata ::
  RoleName -> PermType -> LogicalModelMetadata b -> LogicalModelMetadata b
dropLogicalModelPermissionInMetadata rn = \case
  PTSelect -> lmmSelectPermissions %~ InsOrdHashMap.delete rn
  PTInsert -> error "Not implemented yet"
  PTDelete -> error "Not implemented yet"
  PTUpdate -> error "Not implemented yet"

dropInlineLogicalModelPermissionInMetadata ::
  RoleName -> PermType -> InlineLogicalModelMetadata b -> InlineLogicalModelMetadata b
dropInlineLogicalModelPermissionInMetadata rn = \case
  PTSelect -> ilmmSelectPermissions %~ InsOrdHashMap.delete rn
  PTInsert -> error "Not implemented yet"
  PTDelete -> error "Not implemented yet"
  PTUpdate -> error "Not implemented yet"

dropComputedFieldInMetadata ::
  ComputedFieldName -> TableMetadata b -> TableMetadata b
dropComputedFieldInMetadata name =
  tmComputedFields %~ InsOrdHashMap.delete name

dropEventTriggerInMetadata :: TriggerName -> TableMetadata b -> TableMetadata b
dropEventTriggerInMetadata name =
  tmEventTriggers %~ InsOrdHashMap.delete name

dropRemoteRelationshipInMetadata ::
  RelName -> TableMetadata b -> TableMetadata b
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ InsOrdHashMap.delete name

dropFunctionInMetadata ::
  forall b. (Backend b) => SourceName -> FunctionName b -> MetadataModifier
dropFunctionInMetadata source function =
  MetadataModifier
    $ metaSources
    . ix source
    . toSourceMetadata
    . (smFunctions @b)
    %~ InsOrdHashMap.delete function

dropRemoteSchemaInMetadata :: RemoteSchemaName -> MetadataModifier
dropRemoteSchemaInMetadata name =
  MetadataModifier $ metaRemoteSchemas %~ InsOrdHashMap.delete name

dropRemoteSchemaPermissionInMetadata :: RemoteSchemaName -> RoleName -> MetadataModifier
dropRemoteSchemaPermissionInMetadata remoteSchemaName roleName =
  MetadataModifier $ metaRemoteSchemas . ix remoteSchemaName . rsmPermissions %~ filter ((/=) roleName . _rspmRole)

dropRemoteSchemaRemoteRelationshipInMetadata :: RemoteSchemaName -> G.Name -> RelName -> MetadataModifier
dropRemoteSchemaRemoteRelationshipInMetadata remoteSchemaName typeName relationshipName =
  MetadataModifier
    $ metaRemoteSchemas
    . ix remoteSchemaName
    . rsmRemoteRelationships
    . ix typeName
    . rstrsRelationships
    %~ InsOrdHashMap.delete relationshipName

-- | Encode 'Metadata' to JSON with deterministic ordering (e.g. "version" being at the top).
-- The CLI system stores metadata in files and has option to show changes in git diff style.
-- The diff shouldn't appear different for no metadata changes. So, the ordering of object keys and
-- array elements should  remain consistent across versions of graphql-engine if possible.
--
-- Note: While modifying any part of the code below, make sure the encoded JSON of each type is
-- parsable via its 'FromJSON' instance.
--
-- TODO: we can use 'aeson-pretty' to serialize in a consistent way, and to specify a (partial)
-- order of keys, while getting the benefits of auto-generated To/FromJSON instances.
-- `FromJSON TableMetadata` complicates this though...
--
-- See: https://github.com/hasura/graphql-engine/issues/6348
metadataToOrdJSON :: Metadata -> AO.Value
metadataToOrdJSON
  ( Metadata
      sources
      remoteSchemas
      queryCollections
      allowlist
      customTypes
      actions
      cronTriggers
      endpoints
      apiLimits
      metricsConfig
      inheritedRoles
      introspectionDisabledRoles
      networkConfig
      backendConfigs
      openTelemetryConfig
    ) =
    AO.object
      $ [versionPair, sourcesPair]
      <> catMaybes
        [ remoteSchemasPair,
          queryCollectionsPair,
          allowlistPair,
          actionsPair,
          customTypesPair,
          cronTriggersPair,
          endpointsPair,
          apiLimitsPair,
          metricsConfigPair,
          inheritedRolesPair,
          introspectionDisabledRolesPair,
          networkPair,
          backendConfigsPair,
          openTelemetryConfigPair
        ]
    where
      versionPair = ("version", AO.toOrdered currentMetadataVersion)
      sourcesPair = ("sources", AO.Array $ sourcesToOrdJSONList sources)
      remoteSchemasPair = ("remote_schemas",) . AO.Array <$> remoteSchemasToOrdJSONList remoteSchemas
      queryCollectionsPair = ("query_collections",) . AO.Array <$> queryCollectionsToOrdJSONList queryCollections
      allowlistPair = ("allowlist",) . AO.Array <$> allowlistToOrdJSONList allowlist
      customTypesPair = ("custom_types",) . AO.Object <$> customTypesToOrdJSON customTypes
      actionsPair = ("actions",) . AO.Array <$> actionMetadataToOrdJSONList actions
      cronTriggersPair = ("cron_triggers",) . AO.Array <$> cronTriggersToOrdJSONList cronTriggers
      inheritedRolesPair = ("inherited_roles",) . AO.Array <$> inheritedRolesToOrdJSONList inheritedRoles
      endpointsPair = ("rest_endpoints",) . AO.Array <$> endpointsToOrdJSONList endpoints
      apiLimitsPair = ("api_limits",) <$> apiLimitsToOrdJSON apiLimits
      metricsConfigPair = ("metrics_config",) <$> metricsConfigToOrdJSON metricsConfig
      introspectionDisabledRolesPair =
        ("graphql_schema_introspection",) <$> introspectionDisabledRolesToOrdJSON introspectionDisabledRoles
      networkPair = ("network",) <$> networkConfigToOrdJSON networkConfig
      backendConfigsPair = ("backend_configs",) <$> backendConfigsToOrdJSON backendConfigs
      openTelemetryConfigPair = ("opentelemetry",) <$> openTelemetryConfigToOrdJSON openTelemetryConfig

instance ToJSON Metadata where
  toJSON = AO.fromOrdered . metadataToOrdJSON

instance ToJSON MetadataDefaults where
  toJSON (MetadataDefaults m) = toJSON m

-- | Convert 'Metadata' to a DTO for serialization. In the near future the plan
-- is to use this function instead of the 'ToJSON' instance of 'Metadata'.
-- For the time being DTO serialization does not match the same order of object
-- keys as the 'ToJSON' instance - we can't switch to using this function until
-- that issue is resolved. See https://hasurahq.atlassian.net/browse/MM-29
metadataToDTO :: Metadata -> MetadataV3
metadataToDTO
  ( Metadata
      sources
      remoteSchemas
      queryCollections
      allowlist
      customTypes
      actions
      cronTriggers
      endpoints
      apiLimits
      metricsConfig
      inheritedRoles
      introspectionDisabledRoles
      networkConfig
      backendConfigs
      openTelemetryConfig
    ) =
    MetadataV3
      { metaV3Sources = sources,
        metaV3RemoteSchemas = remoteSchemas,
        metaV3QueryCollections = queryCollections,
        metaV3Allowlist = allowlist,
        metaV3Actions = actions,
        metaV3CustomTypes = customTypes,
        metaV3CronTriggers = cronTriggers,
        metaV3RestEndpoints = endpoints,
        metaV3ApiLimits = apiLimits,
        metaV3MetricsConfig = metricsConfig,
        metaV3InheritedRoles = inheritedRoles,
        metaV3GraphqlSchemaIntrospection = introspectionDisabledRoles,
        metaV3Network = networkConfig,
        metaV3BackendConfigs = backendConfigs,
        metaV3OpenTelemetryConfig = openTelemetryConfig
      }
