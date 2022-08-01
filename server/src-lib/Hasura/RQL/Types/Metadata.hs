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
import Data.Aeson qualified as JSON
import Data.Aeson.Extended (FromJSONWithContext (..), mapWithJSONPath)
import Data.Aeson.Ordered qualified as AO
import Data.Aeson.TH
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.Monoid (Dual (..), Endo (..))
import Data.Text.Extended qualified as T
import Data.Vector qualified as Vector
import Hasura.Metadata.DTO.MetadataV3 (MetadataV3 (..))
import Hasura.Metadata.DTO.Placeholder (IsPlaceholder (placeholder))
import Hasura.Prelude
import Hasura.RQL.Types.Action
  ( ActionDefinition (..),
    ActionDefinitionInput,
    ActionMetadata (..),
    ActionPermissionMetadata (..),
    ActionType (..),
    ArgumentDefinition (..),
  )
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnValues)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata.Common
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection (CreateCollection (..))
import Hasura.RQL.Types.Relationships.Local (RelDef (..))
import Hasura.RQL.Types.Relationships.Remote (RemoteRelationship (..))
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.Roles (InheritedRole, Role (..))
import Hasura.RQL.Types.ScheduledTrigger (CronTriggerMetadata (..), defaultSTRetryConf)
import Hasura.RQL.Types.SourceCustomization (emptySourceCustomization)
import Hasura.RQL.Types.Table (emptyTableConfig)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.SQL.Tag (HasTag (backendTag), reify)
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

sourcesToOrdJSONList :: Sources -> AO.Array
sourcesToOrdJSONList sources =
  Vector.fromList $
    map sourceMetaToOrdJSON $ sortOn getSourceName $ OM.elems sources
  where
    sourceMetaToOrdJSON :: BackendSourceMetadata -> AO.Value
    sourceMetaToOrdJSON exists =
      AB.dispatchAnyBackend @Backend exists $ \(SourceMetadata {..} :: SourceMetadata b) ->
        let sourceNamePair = ("name", AO.toOrdered _smName)
            sourceKindPair = ("kind", AO.toOrdered _smKind)
            tablesPair = ("tables", AO.array $ map tableMetaToOrdJSON $ sortOn _tmTable $ OM.elems _smTables)
            functionsPair = listToMaybeOrdPairSort "functions" functionMetadataToOrdJSON _fmFunction _smFunctions
            configurationPair = [("configuration", AO.toOrdered _smConfiguration)]
            queryTagsConfigPair = maybe [] (\queryTagsConfig -> [("query_tags", AO.toOrdered queryTagsConfig)]) _smQueryTags

            customizationPair =
              guard (_smCustomization /= emptySourceCustomization)
                *> [("customization", AO.toOrdered _smCustomization)]
         in AO.object $ [sourceNamePair, sourceKindPair, tablesPair] <> maybeToList functionsPair <> configurationPair <> queryTagsConfigPair <> customizationPair

    tableMetaToOrdJSON :: (Backend b) => TableMetadata b -> AO.Value
    tableMetaToOrdJSON
      ( TableMetadata
          table
          isEnum
          config
          objectRelationships
          arrayRelationships
          computedFields
          remoteRelationships
          insertPermissions
          selectPermissions
          updatePermissions
          deletePermissions
          eventTriggers
          enableApolloFed
        ) =
        AO.object $
          [("table", AO.toOrdered table)]
            <> catMaybes
              [ isEnumPair,
                configPair,
                objectRelationshipsPair,
                arrayRelationshipsPair,
                computedFieldsPair,
                remoteRelationshipsPair,
                insertPermissionsPair,
                selectPermissionsPair,
                updatePermissionsPair,
                deletePermissionsPair,
                eventTriggersPair,
                apolloFedConfigPair
              ]
        where
          isEnumPair = if isEnum then Just ("is_enum", AO.toOrdered isEnum) else Nothing
          apolloFedConfigPair = fmap (\afConfig -> ("apollo_federation_config", AO.toOrdered afConfig)) enableApolloFed
          configPair =
            if config == emptyTableConfig
              then Nothing
              else Just ("configuration", AO.toOrdered config)
          objectRelationshipsPair =
            listToMaybeOrdPairSort
              "object_relationships"
              relDefToOrdJSON
              _rdName
              objectRelationships
          arrayRelationshipsPair =
            listToMaybeOrdPairSort
              "array_relationships"
              relDefToOrdJSON
              _rdName
              arrayRelationships
          computedFieldsPair =
            listToMaybeOrdPairSort
              "computed_fields"
              computedFieldMetaToOrdJSON
              _cfmName
              computedFields
          remoteRelationshipsPair =
            listToMaybeOrdPairSort
              "remote_relationships"
              AO.toOrdered
              _rrName
              remoteRelationships
          insertPermissionsPair =
            listToMaybeOrdPairSort
              "insert_permissions"
              insPermDefToOrdJSON
              _pdRole
              insertPermissions
          selectPermissionsPair =
            listToMaybeOrdPairSort
              "select_permissions"
              selPermDefToOrdJSON
              _pdRole
              selectPermissions
          updatePermissionsPair =
            listToMaybeOrdPairSort
              "update_permissions"
              updPermDefToOrdJSON
              _pdRole
              updatePermissions
          deletePermissionsPair =
            listToMaybeOrdPairSort
              "delete_permissions"
              delPermDefToOrdJSON
              _pdRole
              deletePermissions
          eventTriggersPair =
            listToMaybeOrdPairSort
              "event_triggers"
              eventTriggerConfToOrdJSON
              etcName
              eventTriggers

          relDefToOrdJSON :: (ToJSON a) => RelDef a -> AO.Value
          relDefToOrdJSON (RelDef name using comment) =
            AO.object $
              [ ("name", AO.toOrdered name),
                ("using", AO.toOrdered using)
              ]
                <> catMaybes [maybeCommentToMaybeOrdPair comment]

          computedFieldMetaToOrdJSON :: (Backend b) => ComputedFieldMetadata b -> AO.Value
          computedFieldMetaToOrdJSON (ComputedFieldMetadata name definition comment) =
            AO.object $
              [ ("name", AO.toOrdered name),
                ("definition", AO.toOrdered definition)
              ]
                <> catMaybes [commentToMaybeOrdPair comment]

          insPermDefToOrdJSON :: forall b. (Backend b) => InsPermDef b -> AO.Value
          insPermDefToOrdJSON = permDefToOrdJSON insPermToOrdJSON
            where
              insPermToOrdJSON (InsPerm check set columns backendOnly) =
                let columnsPair = ("columns",) . AO.toOrdered <$> columns
                    backendOnlyPair =
                      if backendOnly
                        then Just ("backend_only", AO.toOrdered backendOnly)
                        else Nothing
                 in AO.object $
                      [("check", AO.toOrdered check)]
                        <> catMaybes [maybeSetToMaybeOrdPair @b set, columnsPair, backendOnlyPair]

          selPermDefToOrdJSON :: Backend b => SelPermDef b -> AO.Value
          selPermDefToOrdJSON = permDefToOrdJSON selPermToOrdJSON
            where
              selPermToOrdJSON (SelPerm columns fltr limit allowAgg computedFieldsPerm allowedQueryRootFieldTypes allowedSubscriptionRootFieldTypes) =
                AO.object $
                  catMaybes
                    [ columnsPair,
                      computedFieldsPermPair,
                      filterPair,
                      limitPair,
                      allowAggPair,
                      allowedQueryRootFieldsPair,
                      allowedSubscriptionRootFieldsPair
                    ]
                where
                  columnsPair = Just ("columns", AO.toOrdered columns)
                  computedFieldsPermPair = listToMaybeOrdPair "computed_fields" AO.toOrdered computedFieldsPerm
                  filterPair = Just ("filter", AO.toOrdered fltr)
                  limitPair = maybeAnyToMaybeOrdPair "limit" AO.toOrdered limit
                  allowAggPair =
                    if allowAgg
                      then Just ("allow_aggregations", AO.toOrdered allowAgg)
                      else Nothing
                  allowedQueryRootFieldsPair =
                    case allowedQueryRootFieldTypes of
                      ARFAllowAllRootFields -> Nothing
                      ARFAllowConfiguredRootFields configuredRootFields ->
                        Just ("query_root_fields", AO.toOrdered configuredRootFields)
                  allowedSubscriptionRootFieldsPair =
                    case allowedSubscriptionRootFieldTypes of
                      ARFAllowAllRootFields -> Nothing
                      ARFAllowConfiguredRootFields configuredRootFields ->
                        Just ("subscription_root_fields", AO.toOrdered configuredRootFields)

          updPermDefToOrdJSON :: forall b. Backend b => UpdPermDef b -> AO.Value
          updPermDefToOrdJSON = permDefToOrdJSON updPermToOrdJSON
            where
              updPermToOrdJSON (UpdPerm columns set fltr check backendOnly) =
                let backendOnlyPair =
                      if backendOnly
                        then Just ("backend_only", AO.toOrdered backendOnly)
                        else Nothing
                 in AO.object $
                      [ ("columns", AO.toOrdered columns),
                        ("filter", AO.toOrdered fltr),
                        ("check", AO.toOrdered check)
                      ]
                        <> catMaybes [maybeSetToMaybeOrdPair @b set, backendOnlyPair]

          delPermDefToOrdJSON :: Backend b => DelPermDef b -> AO.Value
          delPermDefToOrdJSON = permDefToOrdJSON AO.toOrdered

          permDefToOrdJSON :: (a b -> AO.Value) -> PermDef b a -> AO.Value
          permDefToOrdJSON permToOrdJSON (PermDef role permission comment) =
            AO.object $
              [ ("role", AO.toOrdered role),
                ("permission", permToOrdJSON (unPermDefPermission permission))
              ]
                <> catMaybes [maybeCommentToMaybeOrdPair comment]

          eventTriggerConfToOrdJSON :: Backend b => EventTriggerConf b -> AO.Value
          eventTriggerConfToOrdJSON (EventTriggerConf name definition webhook webhookFromEnv retryConf headers reqTransform respTransform) =
            AO.object $
              [ ("name", AO.toOrdered name),
                ("definition", AO.toOrdered definition),
                ("retry_conf", AO.toOrdered retryConf)
              ]
                <> catMaybes
                  [ maybeAnyToMaybeOrdPair "webhook" AO.toOrdered webhook,
                    maybeAnyToMaybeOrdPair "webhook_from_env" AO.toOrdered webhookFromEnv,
                    headers >>= listToMaybeOrdPair "headers" AO.toOrdered,
                    fmap (("request_transform",) . AO.toOrdered) reqTransform,
                    fmap (("response_transform",) . AO.toOrdered) respTransform
                  ]

    functionMetadataToOrdJSON :: Backend b => FunctionMetadata b -> AO.Value
    functionMetadataToOrdJSON FunctionMetadata {..} =
      let confKeyPair =
            if _fmConfiguration == emptyFunctionConfig
              then []
              else pure ("configuration", AO.toOrdered _fmConfiguration)
          permissionsKeyPair =
            if null _fmPermissions
              then []
              else pure ("permissions", AO.toOrdered _fmPermissions)
          commentKeyPair =
            if isNothing _fmComment
              then []
              else pure ("comment", AO.toOrdered _fmComment)
       in AO.object $ [("function", AO.toOrdered _fmFunction)] <> confKeyPair <> permissionsKeyPair <> commentKeyPair

remoteSchemasToOrdJSONList :: RemoteSchemas -> Maybe AO.Array
remoteSchemasToOrdJSONList = listToMaybeArraySort remoteSchemaQToOrdJSON _rsmName
  where
    remoteSchemaQToOrdJSON :: RemoteSchemaMetadata -> AO.Value
    remoteSchemaQToOrdJSON (RemoteSchemaMetadata name definition comment permissions relationships) =
      AO.object $
        [ ("name", AO.toOrdered name),
          ("definition", remoteSchemaDefToOrdJSON definition)
        ]
          <> catMaybes
            [ maybeCommentToMaybeOrdPair comment,
              listToMaybeOrdPair
                "permissions"
                permsToMaybeOrdJSON
                permissions,
              listToMaybeOrdPair
                "remote_relationships"
                AO.toOrdered
                relationships
            ]
      where
        permsToMaybeOrdJSON :: RemoteSchemaPermissionMetadata -> AO.Value
        permsToMaybeOrdJSON (RemoteSchemaPermissionMetadata role defn permComment) =
          AO.object $
            [ ("role", AO.toOrdered role),
              ("definition", AO.toOrdered defn)
            ]
              <> catMaybes [maybeCommentToMaybeOrdPair permComment]

        remoteSchemaDefToOrdJSON :: RemoteSchemaDef -> AO.Value
        remoteSchemaDefToOrdJSON (RemoteSchemaDef url urlFromEnv headers frwrdClientHdrs timeout customization) =
          AO.object $
            catMaybes
              [ maybeToPair "url" url,
                maybeToPair "url_from_env" urlFromEnv,
                maybeToPair "timeout_seconds" timeout,
                maybeToPair "customization" customization,
                headers >>= listToMaybeOrdPair "headers" AO.toOrdered
              ]
              <> [("forward_client_headers", AO.toOrdered frwrdClientHdrs) | frwrdClientHdrs]
          where
            maybeToPair n = maybeAnyToMaybeOrdPair n AO.toOrdered

backendConfigsToOrdJSON :: BackendMap BackendConfigWrapper -> Maybe AO.Value
backendConfigsToOrdJSON = ifNotEmpty (== mempty) configsToOrdJSON
  where
    configsToOrdJSON :: BackendMap BackendConfigWrapper -> AO.Value
    configsToOrdJSON backendConfigs' =
      AO.object . sortOn fst $ backendConfigToOrdJSON <$> BackendMap.elems backendConfigs'

    backendConfigToOrdJSON :: AB.AnyBackend BackendConfigWrapper -> (Text, AO.Value)
    backendConfigToOrdJSON backendConfig =
      AB.dispatchAnyBackend @Backend backendConfig $ \((BackendConfigWrapper backendConfig') :: BackendConfigWrapper b) ->
        let backendTypeStr = T.toTxt $ reify $ backendTag @b
            val = AO.toOrdered backendConfig'
         in (backendTypeStr, val)

inheritedRolesToOrdJSONList :: InheritedRoles -> Maybe AO.Array
inheritedRolesToOrdJSONList = listToMaybeArraySort inheritedRolesQToOrdJSON _rRoleName
  where
    inheritedRolesQToOrdJSON :: InheritedRole -> AO.Value
    inheritedRolesQToOrdJSON (Role roleName roleSet) =
      AO.object
        [ ("role_name", AO.toOrdered roleName),
          ("role_set", AO.toOrdered roleSet)
        ]

queryCollectionsToOrdJSONList :: QueryCollections -> Maybe AO.Array
queryCollectionsToOrdJSONList = listToMaybeArraySort createCollectionToOrdJSON _ccName
  where
    createCollectionToOrdJSON :: CreateCollection -> AO.Value
    createCollectionToOrdJSON (CreateCollection name definition comment) =
      AO.object $
        [ ("name", AO.toOrdered name),
          ("definition", AO.toOrdered definition)
        ]
          <> catMaybes [maybeCommentToMaybeOrdPair comment]

allowlistToOrdJSONList :: MetadataAllowlist -> Maybe AO.Array
allowlistToOrdJSONList = listToMaybeArraySort (AO.toOrdered . toJSON @AllowlistEntry) aeCollection

apiLimitsToOrdJSON :: ApiLimit -> Maybe AO.Value
apiLimitsToOrdJSON apiLimits
  | apiLimits == emptyApiLimit = Nothing
  | otherwise = Just $ AO.toOrdered apiLimits

cronTriggersToOrdJSONList :: CronTriggers -> Maybe AO.Array
cronTriggersToOrdJSONList = listToMaybeArraySort crontriggerQToOrdJSON ctName
  where
    crontriggerQToOrdJSON :: CronTriggerMetadata -> AO.Value
    crontriggerQToOrdJSON
      (CronTriggerMetadata name webhook schedule payload retryConf headers includeInMetadata comment reqTransform respTransform) =
        AO.object $
          [ ("name", AO.toOrdered name),
            ("webhook", AO.toOrdered webhook),
            ("schedule", AO.toOrdered schedule),
            ("include_in_metadata", AO.toOrdered includeInMetadata)
          ]
            <> catMaybes
              [ maybeAnyToMaybeOrdPair "payload" AO.toOrdered payload,
                maybeAnyToMaybeOrdPair "retry_conf" AO.toOrdered (maybeRetryConfiguration retryConf),
                maybeAnyToMaybeOrdPair "headers" AO.toOrdered (maybeHeader headers),
                maybeAnyToMaybeOrdPair "comment" AO.toOrdered comment,
                fmap (("request_transform",) . AO.toOrdered) reqTransform,
                fmap (("response_transform",) . AO.toOrdered) respTransform
              ]
        where
          maybeRetryConfiguration retryConfig
            | retryConfig == defaultSTRetryConf = Nothing
            | otherwise = Just retryConfig

          maybeHeader headerConfig
            | null headerConfig = Nothing
            | otherwise = Just headerConfig

customTypesToOrdJSON :: CustomTypes -> Maybe AO.Object
customTypesToOrdJSON customTypes@(CustomTypes inpObjs objs scalars enums)
  | customTypes == emptyCustomTypes = Nothing
  | otherwise =
    Just . AO.fromList . catMaybes $
      [ listToMaybeOrdPair "input_objects" inputObjectToOrdJSON inpObjs,
        listToMaybeOrdPair "objects" objectTypeToOrdJSON objs,
        listToMaybeOrdPair "scalars" scalarTypeToOrdJSON scalars,
        listToMaybeOrdPair "enums" enumTypeToOrdJSON enums
      ]
  where
    inputObjectToOrdJSON :: InputObjectTypeDefinition -> AO.Value
    inputObjectToOrdJSON (InputObjectTypeDefinition tyName descM fields) =
      AO.object $
        [ ("name", AO.toOrdered tyName),
          ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
        ]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]
      where
        fieldDefinitionToOrdJSON :: InputObjectFieldDefinition -> AO.Value
        fieldDefinitionToOrdJSON (InputObjectFieldDefinition fieldName fieldDescM ty) =
          AO.object $
            [ ("name", AO.toOrdered fieldName),
              ("type", AO.toOrdered ty)
            ]
              <> catMaybes [maybeDescriptionToMaybeOrdPair fieldDescM]

    objectTypeToOrdJSON :: ObjectTypeDefinition -> AO.Value
    objectTypeToOrdJSON (ObjectTypeDefinition tyName descM fields rels) =
      AO.object $
        [ ("name", AO.toOrdered tyName),
          ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
        ]
          <> catMaybes
            [ maybeDescriptionToMaybeOrdPair descM,
              listToMaybeOrdPair "relationships" AO.toOrdered rels
            ]
      where
        fieldDefinitionToOrdJSON :: ObjectFieldDefinition GraphQLType -> AO.Value
        fieldDefinitionToOrdJSON (ObjectFieldDefinition fieldName argsValM fieldDescM ty) =
          AO.object $
            [ ("name", AO.toOrdered fieldName),
              ("type", AO.toOrdered ty)
            ]
              <> catMaybes
                [ ("arguments",) . AO.toOrdered <$> argsValM,
                  maybeDescriptionToMaybeOrdPair fieldDescM
                ]

    scalarTypeToOrdJSON :: ScalarTypeDefinition -> AO.Value
    scalarTypeToOrdJSON (ScalarTypeDefinition tyName descM) =
      AO.object $
        [("name", AO.toOrdered tyName)]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]

    enumTypeToOrdJSON :: EnumTypeDefinition -> AO.Value
    enumTypeToOrdJSON (EnumTypeDefinition tyName descM values) =
      AO.object $
        [ ("name", AO.toOrdered tyName),
          ("values", AO.toOrdered values)
        ]
          <> catMaybes [maybeDescriptionToMaybeOrdPair descM]

endpointsToOrdJSONList :: Endpoints -> Maybe AO.Array
endpointsToOrdJSONList = listToMaybeArraySort AO.toOrdered _ceUrl

introspectionDisabledRolesToOrdJSON :: SetGraphqlIntrospectionOptions -> Maybe AO.Value
introspectionDisabledRolesToOrdJSON = ifNotEmpty (== mempty) AO.toOrdered

metricsConfigToOrdJSON :: MetricsConfig -> Maybe AO.Value
metricsConfigToOrdJSON = ifNotEmpty (== emptyMetricsConfig) AO.toOrdered

networkConfigToOrdJSON :: Network -> Maybe AO.Value
networkConfigToOrdJSON = ifNotEmpty (== emptyNetwork) AO.toOrdered

actionMetadataToOrdJSONList :: Actions -> Maybe AO.Array
actionMetadataToOrdJSONList = listToMaybeArraySort actionMetadataToOrdJSON _amName
  where
    actionMetadataToOrdJSON :: ActionMetadata -> AO.Value
    actionMetadataToOrdJSON (ActionMetadata name comment definition permissions) =
      AO.object $
        [ ("name", AO.toOrdered name),
          ("definition", actionDefinitionToOrdJSON definition)
        ]
          <> catMaybes
            [ maybeCommentToMaybeOrdPair comment,
              listToMaybeOrdPair "permissions" permToOrdJSON permissions
            ]
      where
        argDefinitionToOrdJSON :: ArgumentDefinition GraphQLType -> AO.Value
        argDefinitionToOrdJSON (ArgumentDefinition argName ty descM) =
          AO.object $
            [ ("name", AO.toOrdered argName),
              ("type", AO.toOrdered ty)
            ]
              <> catMaybes [maybeAnyToMaybeOrdPair "description" AO.toOrdered descM]

        actionDefinitionToOrdJSON :: ActionDefinitionInput -> AO.Value
        actionDefinitionToOrdJSON
          ( ActionDefinition
              args
              outputType
              actionType
              headers
              frwrdClientHdrs
              timeout
              handler
              requestTransform
              responseTransform
            ) =
            let typeAndKind = case actionType of
                  ActionQuery -> [("type", AO.toOrdered ("query" :: String))]
                  ActionMutation kind ->
                    [ ("type", AO.toOrdered ("mutation" :: String)),
                      ("kind", AO.toOrdered kind)
                    ]
             in AO.object $
                  [ ("handler", AO.toOrdered handler),
                    ("output_type", AO.toOrdered outputType)
                  ]
                    <> [("forward_client_headers", AO.toOrdered frwrdClientHdrs) | frwrdClientHdrs]
                    <> catMaybes
                      [ listToMaybeOrdPair "headers" AO.toOrdered headers,
                        listToMaybeOrdPair "arguments" argDefinitionToOrdJSON args,
                        fmap (("request_transform",) . AO.toOrdered) requestTransform,
                        fmap (("response_transform",) . AO.toOrdered) responseTransform
                      ]
                    <> typeAndKind
                    <> bool [("timeout", AO.toOrdered timeout)] mempty (timeout == defaultActionTimeoutSecs)

        permToOrdJSON :: ActionPermissionMetadata -> AO.Value
        permToOrdJSON (ActionPermissionMetadata role permComment) =
          AO.object $ [("role", AO.toOrdered role)] <> catMaybes [maybeCommentToMaybeOrdPair permComment]

ifNotEmpty :: (a -> Bool) -> (a -> b) -> a -> Maybe b
ifNotEmpty isEmpty f x
  | isEmpty x = Nothing
  | otherwise = Just $ f x

-- | Sort list before encoding to JSON value
listToMaybeOrdPairSort ::
  (Foldable t, Ord b) =>
  Text ->
  (a -> AO.Value) ->
  (a -> b) ->
  t a ->
  Maybe (Text, AO.Value)
listToMaybeOrdPairSort name f sortF ta = case toList ta of
  [] -> Nothing
  list -> Just $ (name,) $ AO.array $ map f $ sortOn sortF list

-- | Sort list before encoding to JSON array (not value)
listToMaybeArraySort ::
  (Foldable t, Ord b) =>
  (a -> AO.Value) ->
  (a -> b) ->
  t a ->
  Maybe AO.Array
listToMaybeArraySort f sortF ta = case toList ta of
  [] -> Nothing
  list -> Just $ Vector.fromList $ map f $ sortOn sortF list

listToMaybeOrdPair ::
  (Foldable t) =>
  Text ->
  (a -> AO.Value) ->
  t a ->
  Maybe (Text, AO.Value)
listToMaybeOrdPair name f ta = case toList ta of
  [] -> Nothing
  list -> Just $ (name,) $ AO.array $ map f list

maybeSetToMaybeOrdPair :: (Backend b) => Maybe (ColumnValues b JSON.Value) -> Maybe (Text, AO.Value)
maybeSetToMaybeOrdPair set =
  set >>= \colVals ->
    if colVals == mempty
      then Nothing
      else Just ("set", AO.toOrdered colVals)

maybeDescriptionToMaybeOrdPair :: Maybe G.Description -> Maybe (Text, AO.Value)
maybeDescriptionToMaybeOrdPair = maybeAnyToMaybeOrdPair "description" AO.toOrdered

maybeCommentToMaybeOrdPair :: Maybe Text -> Maybe (Text, AO.Value)
maybeCommentToMaybeOrdPair = maybeAnyToMaybeOrdPair "comment" AO.toOrdered

maybeAnyToMaybeOrdPair :: Text -> (a -> AO.Value) -> Maybe a -> Maybe (Text, AO.Value)
maybeAnyToMaybeOrdPair name f = fmap ((name,) . f)

commentToMaybeOrdPair :: Comment -> Maybe (Text, AO.Value)
commentToMaybeOrdPair comment = (\val -> ("comment", AO.toOrdered val)) <$> commentToMaybeText comment
