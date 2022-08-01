{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Metadata
  ( Metadata (..),
    MetadataM (..),
    MetadataModifier (..),
    MetadataNoSources (..),
    MetadataVersion (..),
    currentMetadataVersion,
    dropComputedFieldInMetadata,
    dropEventTriggerInMetadata,
    dropFunctionInMetadata,
    dropPermissionInMetadata,
    dropRelationshipInMetadata,
    dropRemoteRelationshipInMetadata,
    dropTableInMetadata,
    dropRemoteSchemaInMetadata,
    dropRemoteSchemaPermissionInMetadata,
    dropRemoteSchemaRemoteRelationshipInMetadata,
    emptyMetadata,
    functionMetadataSetter,
    metaActions,
    metaAllowlist,
    metaApiLimits,
    metaBackendConfigs,
    metaCronTriggers,
    metaCustomTypes,
    metaInheritedRoles,
    metaMetricsConfig,
    metaNetwork,
    metaQueryCollections,
    metaRemoteSchemas,
    metaRestEndpoints,
    metaSetGraphqlIntrospectionOptions,
    metaSources,
    metadataToDTO,
    metadataToOrdJSON,
    tableMetadataSetter,
    module Hasura.RQL.Types.Metadata.Common,
  )
where

import Control.Lens hiding (set, (.=))
import Data.Aeson.Extended (FromJSONWithContext (..), mapWithJSONPath)
import Data.Aeson.Ordered qualified as AO
import Data.Aeson.TH
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.Monoid (Dual (..), Endo (..))
import Hasura.Metadata.DTO.MetadataV3 (MetadataV3 (..))
import Hasura.Metadata.DTO.Placeholder (IsPlaceholder (placeholder))
import Hasura.Prelude
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata.Common
import Hasura.RQL.Types.Metadata.Serialization
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.RemoteSchema
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.Session
import Hasura.Tracing (TraceT)
import Language.GraphQL.Draft.Syntax qualified as G

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
    _metaBackendConfigs :: BackendMap BackendConfigWrapper
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''Metadata)

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    when (version /= MVVersion3) $
      fail $ "unexpected metadata version from storage: " <> show version
    rawSources <- o .: "sources"
    backendConfigs <- o .:? "backend_configs" .!= mempty
    sources <- oMapFromL getSourceName <$> mapWithJSONPath parseSourceMetadata rawSources <?> Key "sources"
    endpoints <- oMapFromL _ceName <$> o .:? "rest_endpoints" .!= []
    network <- o .:? "network" .!= emptyNetwork
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
    pure $
      Metadata
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
    where
      parseSourceMetadata :: Value -> Parser (AB.AnyBackend SourceMetadata)
      parseSourceMetadata = withObject "SourceMetadata" \o -> do
        backendSourceKind <- explicitParseFieldMaybe AB.parseBackendSourceKindFromJSON o "kind" .!= AB.mkAnyBackend PostgresVanillaKind
        AB.dispatchAnyBackend @Backend
          backendSourceKind
          ( \(kind :: BackendSourceKind b) ->
              AB.mkAnyBackend @b <$> parseJSONWithContext kind (Object o)
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
      _metaBackendConfigs = mempty
    }

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
  deriving (Eq)

$(deriveToJSON hasuraJSON ''MetadataNoSources)

instance FromJSON MetadataNoSources where
  parseJSON = withObject "MetadataNoSources" $ \o -> do
    version <- o .:? "version" .!= MVVersion1
    (tables, functions) <-
      case version of
        MVVersion1 -> do
          tables <- oMapFromL _tmTable <$> o .: "tables"
          functionList <- o .:? "functions" .!= []
          let functions = OM.fromList $
                flip map functionList $
                  \function -> (function, FunctionMetadata function emptyFunctionConfig mempty Nothing)
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
    pure $
      MetadataNoSources
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
  MetadataModifier $ metaSources . ix source . (toSourceMetadata @b) . smTables %~ OM.delete table

dropRelationshipInMetadata ::
  RelName -> TableMetadata b -> TableMetadata b
dropRelationshipInMetadata relName =
  -- Since the name of a relationship is unique in a table, the relationship
  -- with given name may present in either array or object relationships but
  -- not in both.
  (tmObjectRelationships %~ OM.delete relName)
    . (tmArrayRelationships %~ OM.delete relName)

dropPermissionInMetadata ::
  RoleName -> PermType -> TableMetadata b -> TableMetadata b
dropPermissionInMetadata rn = \case
  PTInsert -> tmInsertPermissions %~ OM.delete rn
  PTSelect -> tmSelectPermissions %~ OM.delete rn
  PTDelete -> tmDeletePermissions %~ OM.delete rn
  PTUpdate -> tmUpdatePermissions %~ OM.delete rn

dropComputedFieldInMetadata ::
  ComputedFieldName -> TableMetadata b -> TableMetadata b
dropComputedFieldInMetadata name =
  tmComputedFields %~ OM.delete name

dropEventTriggerInMetadata :: TriggerName -> TableMetadata b -> TableMetadata b
dropEventTriggerInMetadata name =
  tmEventTriggers %~ OM.delete name

dropRemoteRelationshipInMetadata ::
  RelName -> TableMetadata b -> TableMetadata b
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ OM.delete name

dropFunctionInMetadata ::
  forall b. (Backend b) => SourceName -> FunctionName b -> MetadataModifier
dropFunctionInMetadata source function =
  MetadataModifier $
    metaSources . ix source . toSourceMetadata . (smFunctions @b) %~ OM.delete function

dropRemoteSchemaInMetadata :: RemoteSchemaName -> MetadataModifier
dropRemoteSchemaInMetadata name =
  MetadataModifier $ metaRemoteSchemas %~ OM.delete name

dropRemoteSchemaPermissionInMetadata :: RemoteSchemaName -> RoleName -> MetadataModifier
dropRemoteSchemaPermissionInMetadata remoteSchemaName roleName =
  MetadataModifier $ metaRemoteSchemas . ix remoteSchemaName . rsmPermissions %~ filter ((/=) roleName . _rspmRole)

dropRemoteSchemaRemoteRelationshipInMetadata :: RemoteSchemaName -> G.Name -> RelName -> MetadataModifier
dropRemoteSchemaRemoteRelationshipInMetadata remoteSchemaName typeName relationshipName =
  MetadataModifier $
    metaRemoteSchemas
      . ix remoteSchemaName
      . rsmRemoteRelationships
      . ix typeName
      . rstrsRelationships
      %~ OM.delete relationshipName

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
    ) =
    AO.object $
      [versionPair, sourcesPair]
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
            backendConfigsPair
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

instance ToJSON Metadata where
  toJSON = AO.fromOrdered . metadataToOrdJSON

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
    ) =
    MetadataV3
      { metaV3Sources = placeholder $ sourcesToOrdJSONList sources,
        metaV3RemoteSchemas = placeholder <$> remoteSchemasToOrdJSONList remoteSchemas,
        metaV3QueryCollections = placeholder <$> queryCollectionsToOrdJSONList queryCollections,
        metaV3Allowlist = placeholder <$> allowlistToOrdJSONList allowlist,
        metaV3Actions = placeholder <$> actionMetadataToOrdJSONList actions,
        metaV3CustomTypes = placeholder <$> customTypesToOrdJSON customTypes,
        metaV3CronTriggers = placeholder <$> cronTriggersToOrdJSONList cronTriggers,
        metaV3RestEndpoints = placeholder <$> endpointsToOrdJSONList endpoints,
        metaV3ApiLimits = placeholder . objectFromOrdJSON <$> apiLimitsToOrdJSON apiLimits,
        metaV3MetricsConfig = placeholder . objectFromOrdJSON <$> metricsConfigToOrdJSON metricsConfig,
        metaV3InheritedRoles = placeholder <$> inheritedRolesToOrdJSONList inheritedRoles,
        metaV3GraphqlSchemaIntrospection = placeholder . objectFromOrdJSON <$> introspectionDisabledRolesToOrdJSON introspectionDisabledRoles,
        metaV3Network = placeholder . objectFromOrdJSON <$> networkConfigToOrdJSON networkConfig,
        metaV3BackendConfigs = placeholder . objectFromOrdJSON <$> backendConfigsToOrdJSON backendConfigs
      }
    where
      -- This is a /partial/ function to unwrap a JSON object
      objectFromOrdJSON (AO.Object obj) = obj
      objectFromOrdJSON _ = error "expected an object"
