{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Metadata
  ( Actions,
    BackendConfigWrapper (..),
    BackendSourceMetadata,
    CatalogState (..),
    CatalogStateType (..),
    ComputedFieldMetadata (..),
    ComputedFields,
    CronTriggers,
    Endpoints,
    EventTriggers,
    FunctionMetadata (..),
    Functions,
    GetCatalogState (..),
    InheritedRoles,
    Metadata (..),
    MetadataM (..),
    MetadataModifier (..),
    MetadataNoSources (..),
    MetadataVersion (..),
    Permissions,
    QueryCollections,
    Relationships,
    SchemaRemoteRelationships,
    RemoteSchemaTypeRelationships (..),
    rstrsName,
    rstrsRelationships,
    RemoteSchemaMetadata (..),
    RemoteSchemaPermissionMetadata (..),
    RemoteSchemas,
    SetCatalogState (..),
    SourceMetadata (..),
    Sources,
    TableMetadata (..),
    Tables,
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
    fmComment,
    fmConfiguration,
    fmFunction,
    fmPermissions,
    functionMetadataSetter,
    getSourceName,
    metaActions,
    metaAllowlist,
    metaApiLimits,
    metaCronTriggers,
    metaCustomTypes,
    metaBackendConfigs,
    metaInheritedRoles,
    metaMetricsConfig,
    metaNetwork,
    metaQueryCollections,
    metaRemoteSchemas,
    metaRestEndpoints,
    metaSetGraphqlIntrospectionOptions,
    metaSources,
    metadataToOrdJSON,
    mkSourceMetadata,
    mkTableMeta,
    parseNonSourcesMetadata,
    rsmComment,
    rsmDefinition,
    rsmName,
    rsmPermissions,
    rspmComment,
    rspmDefinition,
    rspmRole,
    smConfiguration,
    smFunctions,
    smKind,
    smName,
    smQueryTags,
    smTables,
    smCustomization,
    tableMetadataSetter,
    tmArrayRelationships,
    tmComputedFields,
    tmConfiguration,
    tmDeletePermissions,
    tmEventTriggers,
    tmInsertPermissions,
    tmIsEnum,
    tmObjectRelationships,
    tmRemoteRelationships,
    tmSelectPermissions,
    tmTable,
    tmUpdatePermissions,
    toSourceMetadata,
    rsmRemoteRelationships,
  )
where

import Control.Lens hiding (set, (.=))
import Data.Aeson.Casing
import Data.Aeson.Extended (FromJSONWithContext (..), mapWithJSONPath)
import Data.Aeson.Ordered qualified as AO
import Data.Aeson.TH
import Data.Aeson.Types
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.HashSet qualified as HS
import Data.List.Extended qualified as L
import Data.Monoid (Dual (..), Endo (..))
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.QueryTags
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.SQL.Tag
import Hasura.Session
import Hasura.Tracing (TraceT)
import Language.GraphQL.Draft.Syntax qualified as G

-- | Parse a list of objects into a map from a derived key,
-- failing if the list has duplicates.
parseListAsMap ::
  (Hashable k, Eq k, T.ToTxt k) =>
  Text ->
  (a -> k) ->
  Parser [a] ->
  Parser (InsOrdHashMap k a)
parseListAsMap things mapFn listP = do
  list <- listP
  let duplicates = toList $ L.duplicates $ map mapFn list
  unless (null duplicates) $
    fail $
      T.unpack $
        "multiple declarations exist for the following " <> things <> ": "
          <> T.commaSeparated duplicates
  pure $ oMapFromL mapFn list

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

data ComputedFieldMetadata b = ComputedFieldMetadata
  { _cfmName :: !ComputedFieldName,
    _cfmDefinition :: !(ComputedFieldDefinition b),
    _cfmComment :: !Comment
  }
  deriving (Generic)

deriving instance (Backend b) => Show (ComputedFieldMetadata b)

deriving instance (Backend b) => Eq (ComputedFieldMetadata b)

instance (Backend b) => Cacheable (ComputedFieldMetadata b)

instance (Backend b) => ToJSON (ComputedFieldMetadata b) where
  toJSON ComputedFieldMetadata {..} =
    object
      [ "name" .= _cfmName,
        "definition" .= _cfmDefinition,
        "comment" .= _cfmComment
      ]

instance (Backend b) => FromJSON (ComputedFieldMetadata b) where
  parseJSON = withObject "ComputedFieldMetadata" $ \obj ->
    ComputedFieldMetadata
      <$> obj .: "name"
      <*> obj .: "definition"
      <*> obj .:? "comment" .!= Automatic

data RemoteSchemaPermissionMetadata = RemoteSchemaPermissionMetadata
  { _rspmRole :: !RoleName,
    _rspmDefinition :: !RemoteSchemaPermissionDefinition,
    _rspmComment :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Cacheable RemoteSchemaPermissionMetadata

$(deriveJSON hasuraJSON {omitNothingFields = True} ''RemoteSchemaPermissionMetadata)
$(makeLenses ''RemoteSchemaPermissionMetadata)

type Relationships a = InsOrdHashMap RelName a

type ComputedFields b = InsOrdHashMap ComputedFieldName (ComputedFieldMetadata b)

type RemoteRelationships = InsOrdHashMap RelName RemoteRelationship

type SchemaRemoteRelationships = InsOrdHashMap G.Name RemoteSchemaTypeRelationships

type Permissions a = InsOrdHashMap RoleName a

type EventTriggers b = InsOrdHashMap TriggerName (EventTriggerConf b)

data RemoteSchemaTypeRelationships = RemoteSchemaTypeRelationships
  { _rstrsName :: G.Name,
    _rstrsRelationships :: RemoteRelationships
  }
  deriving (Show, Eq, Generic)

instance FromJSON RemoteSchemaTypeRelationships where
  parseJSON = withObject "RemoteSchemaMetadata" \obj ->
    RemoteSchemaTypeRelationships
      <$> obj .: "type_name"
      <*> (oMapFromL _rrName <$> obj .:? "relationships" .!= [])

instance ToJSON RemoteSchemaTypeRelationships where
  toJSON RemoteSchemaTypeRelationships {..} =
    object
      [ "type_name" .= _rstrsName,
        "relationships" .= OM.elems _rstrsRelationships
      ]

instance Cacheable RemoteSchemaTypeRelationships

data RemoteSchemaMetadata = RemoteSchemaMetadata
  { _rsmName :: RemoteSchemaName,
    _rsmDefinition :: RemoteSchemaDef,
    _rsmComment :: Maybe Text,
    _rsmPermissions :: [RemoteSchemaPermissionMetadata],
    _rsmRemoteRelationships :: SchemaRemoteRelationships
  }
  deriving (Show, Eq, Generic)

instance Cacheable RemoteSchemaMetadata

instance FromJSON RemoteSchemaMetadata where
  parseJSON = withObject "RemoteSchemaMetadata" \obj ->
    RemoteSchemaMetadata
      <$> obj .: "name"
      <*> obj .: "definition"
      <*> obj .:? "comment"
      <*> obj .:? "permissions" .!= mempty
      <*> (oMapFromL _rstrsName <$> obj .:? "remote_relationships" .!= [])

instance ToJSON RemoteSchemaMetadata where
  toJSON RemoteSchemaMetadata {..} =
    object
      [ "name" .= _rsmName,
        "definition" .= _rsmDefinition,
        "comment" .= _rsmComment,
        "permissions" .= _rsmPermissions,
        "remote_relationships" .= OM.elems _rsmRemoteRelationships
      ]

$(makeLenses ''RemoteSchemaTypeRelationships)
$(makeLenses ''RemoteSchemaMetadata)

data TableMetadata b = TableMetadata
  { _tmTable :: !(TableName b),
    _tmIsEnum :: !Bool,
    _tmConfiguration :: !(TableConfig b),
    _tmObjectRelationships :: !(Relationships (ObjRelDef b)),
    _tmArrayRelationships :: !(Relationships (ArrRelDef b)),
    _tmComputedFields :: !(ComputedFields b),
    _tmRemoteRelationships :: !RemoteRelationships,
    _tmInsertPermissions :: !(Permissions (InsPermDef b)),
    _tmSelectPermissions :: !(Permissions (SelPermDef b)),
    _tmUpdatePermissions :: !(Permissions (UpdPermDef b)),
    _tmDeletePermissions :: !(Permissions (DelPermDef b)),
    _tmEventTriggers :: !(EventTriggers b)
  }
  deriving (Generic)

deriving instance (Backend b) => Show (TableMetadata b)

deriving instance (Backend b) => Eq (TableMetadata b)

instance (Backend b) => Cacheable (TableMetadata b)

instance (Backend b) => ToJSON (TableMetadata b) where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''TableMetadata)

mkTableMeta :: TableName b -> Bool -> TableConfig b -> TableMetadata b
mkTableMeta qt isEnum config =
  TableMetadata
    qt
    isEnum
    config
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

instance (Backend b) => FromJSON (TableMetadata b) where
  parseJSON = withObject "Object" $ \o -> do
    let unexpectedKeys = getUnexpectedKeys o
    unless (null unexpectedKeys) $
      fail $
        "unexpected keys when parsing TableMetadata : "
          <> show (HS.toList unexpectedKeys)

    TableMetadata
      <$> o .: tableKey
      <*> o .:? isEnumKey .!= False
      <*> o .:? configKey .!= emptyTableConfig
      <*> parseListAsMap "object relationships" _rdName (o .:? orKey .!= [])
      <*> parseListAsMap "array relationships" _rdName (o .:? arKey .!= [])
      <*> parseListAsMap "computed fields" _cfmName (o .:? cfKey .!= [])
      <*> parseListAsMap "remote relationships" _rrName (o .:? rrKey .!= [])
      <*> parseListAsMap "insert permissions" _pdRole (o .:? ipKey .!= [])
      <*> parseListAsMap "select permissions" _pdRole (o .:? spKey .!= [])
      <*> parseListAsMap "update permissions" _pdRole (o .:? upKey .!= [])
      <*> parseListAsMap "delete permissions" _pdRole (o .:? dpKey .!= [])
      <*> parseListAsMap "event triggers" etcName (o .:? etKey .!= [])
    where
      tableKey = "table"
      isEnumKey = "is_enum"
      configKey = "configuration"
      orKey = "object_relationships"
      arKey = "array_relationships"
      ipKey = "insert_permissions"
      spKey = "select_permissions"
      upKey = "update_permissions"
      dpKey = "delete_permissions"
      etKey = "event_triggers"
      cfKey = "computed_fields"
      rrKey = "remote_relationships"

      getUnexpectedKeys o =
        HS.fromList (M.keys o) `HS.difference` expectedKeySet

      expectedKeySet =
        HS.fromList
          [ tableKey,
            isEnumKey,
            configKey,
            orKey,
            arKey,
            ipKey,
            spKey,
            upKey,
            dpKey,
            etKey,
            cfKey,
            rrKey
          ]

data FunctionMetadata b = FunctionMetadata
  { _fmFunction :: !(FunctionName b),
    _fmConfiguration :: !FunctionConfig,
    _fmPermissions :: ![FunctionPermissionInfo],
    _fmComment :: !(Maybe Text)
  }
  deriving (Generic)

deriving instance (Backend b) => Show (FunctionMetadata b)

deriving instance (Backend b) => Eq (FunctionMetadata b)

instance (Backend b) => Cacheable (FunctionMetadata b)

instance (Backend b) => ToJSON (FunctionMetadata b) where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''FunctionMetadata)

instance (Backend b) => FromJSON (FunctionMetadata b) where
  parseJSON = withObject "FunctionMetadata" $ \o ->
    FunctionMetadata
      <$> o .: "function"
      <*> o .:? "configuration" .!= emptyFunctionConfig
      <*> o .:? "permissions" .!= []
      <*> o .:? "comment"

type Tables b = InsOrdHashMap (TableName b) (TableMetadata b)

type Functions b = InsOrdHashMap (FunctionName b) (FunctionMetadata b)

type RemoteSchemas = InsOrdHashMap RemoteSchemaName RemoteSchemaMetadata

type Endpoints = InsOrdHashMap EndpointName CreateEndpoint

type Actions = InsOrdHashMap ActionName ActionMetadata

type CronTriggers = InsOrdHashMap TriggerName CronTriggerMetadata

type InheritedRoles = InsOrdHashMap RoleName InheritedRole

data SourceMetadata b = SourceMetadata
  { _smName :: !SourceName,
    _smKind :: !(BackendSourceKind b),
    _smTables :: !(Tables b),
    _smFunctions :: !(Functions b),
    _smConfiguration :: !(SourceConnConfiguration b),
    _smQueryTags :: !(Maybe QueryTagsConfig),
    _smCustomization :: !SourceCustomization
  }
  deriving (Generic)

$(makeLenses ''SourceMetadata)

deriving instance (Backend b) => Show (SourceMetadata b)

deriving instance (Backend b) => Eq (SourceMetadata b)

instance (Backend b) => Cacheable (SourceMetadata b)

instance (Backend b) => FromJSONWithContext (BackendSourceKind b) (SourceMetadata b) where
  parseJSONWithContext _smKind = withObject "Object" $ \o -> do
    _smName <- o .: "name"
    _smTables <- oMapFromL _tmTable <$> o .: "tables"
    _smFunctions <- oMapFromL _fmFunction <$> o .:? "functions" .!= []
    _smConfiguration <- o .: "configuration"
    _smQueryTags <- o .:? "query_tags"
    _smCustomization <- o .:? "customization" .!= emptySourceCustomization
    pure SourceMetadata {..}

mkSourceMetadata ::
  forall (b :: BackendType).
  Backend b =>
  SourceName ->
  BackendSourceKind b ->
  SourceConnConfiguration b ->
  SourceCustomization ->
  BackendSourceMetadata
mkSourceMetadata name backendSourceKind config customization =
  AB.mkAnyBackend $ SourceMetadata @b name backendSourceKind mempty mempty config Nothing customization

type BackendSourceMetadata = AB.AnyBackend SourceMetadata

toSourceMetadata :: forall b. (Backend b) => Prism' BackendSourceMetadata (SourceMetadata b)
toSourceMetadata = prism' AB.mkAnyBackend AB.unpackAnyBackend

getSourceName :: BackendSourceMetadata -> SourceName
getSourceName e = AB.dispatchAnyBackend @Backend e _smName

type Sources = InsOrdHashMap SourceName BackendSourceMetadata

parseNonSourcesMetadata ::
  Object ->
  Parser
    ( RemoteSchemas,
      QueryCollections,
      MetadataAllowlist,
      CustomTypes,
      Actions,
      CronTriggers,
      ApiLimit,
      MetricsConfig,
      InheritedRoles,
      SetGraphqlIntrospectionOptions
    )
parseNonSourcesMetadata o = do
  remoteSchemas <-
    parseListAsMap "remote schemas" _rsmName $
      o .:? "remote_schemas" .!= []
  queryCollections <-
    parseListAsMap "query collections" _ccName $
      o .:? "query_collections" .!= []
  allowlist <- parseListAsMap "allowlist entries" aeCollection $ o .:? "allowlist" .!= []
  customTypes <- o .:? "custom_types" .!= emptyCustomTypes
  actions <- parseListAsMap "actions" _amName $ o .:? "actions" .!= []
  cronTriggers <-
    parseListAsMap "cron triggers" ctName $
      o .:? "cron_triggers" .!= []

  apiLimits <- o .:? "api_limits" .!= emptyApiLimit
  metricsConfig <- o .:? "metrics_config" .!= emptyMetricsConfig
  inheritedRoles <-
    parseListAsMap "inherited roles" _rRoleName $
      o .:? "inherited_roles" .!= []
  introspectionDisabledForRoles <- o .:? "graphql_schema_introspection" .!= mempty
  pure
    ( remoteSchemas,
      queryCollections,
      allowlist,
      customTypes,
      actions,
      cronTriggers,
      apiLimits,
      metricsConfig,
      inheritedRoles,
      introspectionDisabledForRoles
    )

-- | This newtype simply wraps the BackendConfig type family so that it can be used
-- with BackendMap in the Metadata type. GHC will not allow the type family to be
-- used directly. :(
newtype BackendConfigWrapper b = BackendConfigWrapper {unBackendConfigWrapper :: BackendConfig b}

deriving newtype instance (Backend b) => Show (BackendConfigWrapper b)

deriving newtype instance (Backend b) => Eq (BackendConfigWrapper b)

deriving newtype instance (Backend b) => ToJSON (BackendConfigWrapper b)

deriving newtype instance (Backend b) => FromJSON (BackendConfigWrapper b)

-- | A complete GraphQL Engine metadata representation to be stored,
-- exported/replaced via metadata queries.
data Metadata = Metadata
  { _metaSources :: !Sources,
    _metaRemoteSchemas :: !RemoteSchemas,
    _metaQueryCollections :: !QueryCollections,
    _metaAllowlist :: !MetadataAllowlist,
    _metaCustomTypes :: !CustomTypes,
    _metaActions :: !Actions,
    _metaCronTriggers :: !CronTriggers,
    _metaRestEndpoints :: !Endpoints,
    _metaApiLimits :: !ApiLimit,
    _metaMetricsConfig :: !MetricsConfig,
    _metaInheritedRoles :: !InheritedRoles,
    _metaSetGraphqlIntrospectionOptions :: !SetGraphqlIntrospectionOptions,
    _metaNetwork :: !Network,
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
  { _mnsTables :: !(Tables ('Postgres 'Vanilla)),
    _mnsFunctions :: !(Functions ('Postgres 'Vanilla)),
    _mnsRemoteSchemas :: !RemoteSchemas,
    _mnsQueryCollections :: !QueryCollections,
    _mnsAllowlist :: !MetadataAllowlist,
    _mnsCustomTypes :: !CustomTypes,
    _mnsActions :: !Actions,
    _mnsCronTriggers :: !CronTriggers
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
      sourcesPair =
        ("sources", AO.array $ map sourceMetaToOrdJSON $ sortOn getSourceName $ OM.elems sources)
      remoteSchemasPair = listToMaybeOrdPairSort "remote_schemas" remoteSchemaQToOrdJSON _rsmName remoteSchemas
      queryCollectionsPair = listToMaybeOrdPairSort "query_collections" createCollectionToOrdJSON _ccName queryCollections
      allowlistPair = listToMaybeOrdPairSort "allowlist" (AO.toOrdered . toJSON @AllowlistEntry) aeCollection allowlist
      customTypesPair =
        if customTypes == emptyCustomTypes
          then Nothing
          else Just ("custom_types", customTypesToOrdJSON customTypes)
      actionsPair = listToMaybeOrdPairSort "actions" actionMetadataToOrdJSON _amName actions
      cronTriggersPair = listToMaybeOrdPairSort "cron_triggers" crontriggerQToOrdJSON ctName cronTriggers
      inheritedRolesPair = listToMaybeOrdPairSort "inherited_roles" inheritedRolesQToOrdJSON _rRoleName inheritedRoles
      endpointsPair = listToMaybeOrdPairSort "rest_endpoints" AO.toOrdered _ceUrl endpoints

      apiLimitsPair =
        if apiLimits == emptyApiLimit
          then Nothing
          else Just ("api_limits", AO.toOrdered apiLimits)

      metricsConfigPair =
        if metricsConfig == emptyMetricsConfig
          then Nothing
          else Just ("metrics_config", AO.toOrdered metricsConfig)

      introspectionDisabledRolesPair =
        bool
          (Just ("graphql_schema_introspection", AO.toOrdered introspectionDisabledRoles))
          Nothing
          (introspectionDisabledRoles == mempty)

      networkPair =
        if networkConfig /= emptyNetwork
          then Just ("network", AO.toOrdered networkConfig)
          else Nothing

      backendConfigsPair =
        if backendConfigs /= mempty
          then Just ("backend_configs", backendConfigsToOrdJSON backendConfigs)
          else Nothing

      backendConfigsToOrdJSON :: BackendMap BackendConfigWrapper -> AO.Value
      backendConfigsToOrdJSON backendConfigs' =
        AO.object . sortOn fst $ backendConfigToOrdJSON <$> BackendMap.elems backendConfigs'

      backendConfigToOrdJSON :: AB.AnyBackend BackendConfigWrapper -> (Text, AO.Value)
      backendConfigToOrdJSON backendConfig =
        AB.dispatchAnyBackend @Backend backendConfig $ \((BackendConfigWrapper backendConfig') :: BackendConfigWrapper b) ->
          let backendTypeStr = T.toTxt $ reify $ backendTag @b
              val = AO.toOrdered backendConfig'
           in (backendTypeStr, val)

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
                  eventTriggersPair
                ]
          where
            isEnumPair = if isEnum then Just ("is_enum", AO.toOrdered isEnum) else Nothing
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
                selPermToOrdJSON (SelPerm columns fltr limit allowAgg computedFieldsPerm) =
                  AO.object $
                    catMaybes
                      [ columnsPair,
                        computedFieldsPermPair,
                        filterPair,
                        limitPair,
                        allowAggPair
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

      inheritedRolesQToOrdJSON :: InheritedRole -> AO.Value
      inheritedRolesQToOrdJSON (Role roleName roleSet) =
        AO.object
          [ ("role_name", AO.toOrdered roleName),
            ("role_set", AO.toOrdered roleSet)
          ]

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

      createCollectionToOrdJSON :: CreateCollection -> AO.Value
      createCollectionToOrdJSON (CreateCollection name definition comment) =
        AO.object $
          [ ("name", AO.toOrdered name),
            ("definition", AO.toOrdered definition)
          ]
            <> catMaybes [maybeCommentToMaybeOrdPair comment]

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

      customTypesToOrdJSON :: CustomTypes -> AO.Value
      customTypesToOrdJSON (CustomTypes inpObjs objs scalars enums) =
        AO.object . catMaybes $
          [ listToMaybeOrdPair "input_objects" inputObjectToOrdJSON =<< inpObjs,
            listToMaybeOrdPair "objects" objectTypeToOrdJSON =<< objs,
            listToMaybeOrdPair "scalars" scalarTypeToOrdJSON =<< scalars,
            listToMaybeOrdPair "enums" enumTypeToOrdJSON =<< enums
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

          objectTypeToOrdJSON :: ObjectType -> AO.Value
          objectTypeToOrdJSON (ObjectTypeDefinition tyName descM fields rels) =
            AO.object $
              [ ("name", AO.toOrdered tyName),
                ("fields", AO.array $ map fieldDefinitionToOrdJSON $ toList fields)
              ]
                <> catMaybes
                  [ maybeDescriptionToMaybeOrdPair descM,
                    maybeAnyToMaybeOrdPair "relationships" AO.toOrdered rels
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

      -- Utility functions

      -- Sort list before encoding to JSON value
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

      listToMaybeOrdPair ::
        (Foldable t) =>
        Text ->
        (a -> AO.Value) ->
        t a ->
        Maybe (Text, AO.Value)
      listToMaybeOrdPair name f ta = case toList ta of
        [] -> Nothing
        list -> Just $ (name,) $ AO.array $ map f list

      maybeSetToMaybeOrdPair :: (Backend b) => Maybe (ColumnValues b Value) -> Maybe (Text, AO.Value)
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

instance ToJSON Metadata where
  toJSON = AO.fromOrdered . metadataToOrdJSON

data CatalogStateType
  = CSTCli
  | CSTConsole
  deriving (Show, Eq)

$(deriveJSON defaultOptions {constructorTagModifier = snakeCase . drop 3} ''CatalogStateType)

data SetCatalogState = SetCatalogState
  { _scsType :: !CatalogStateType,
    _scsState :: !Value
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON ''SetCatalogState)

data CatalogState = CatalogState
  { _csId :: !Text,
    _csCliState :: !Value,
    _csConsoleState :: !Value
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''CatalogState)

data GetCatalogState
  = GetCatalogState
  deriving (Show, Eq)

$(deriveToJSON defaultOptions ''GetCatalogState)

instance FromJSON GetCatalogState where
  parseJSON _ = pure GetCatalogState
