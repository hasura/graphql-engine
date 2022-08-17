{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | In order to avoid circular dependencies while splitting
-- 'Hasura.RQL.Types.Metadata' into multiple modules, some definitions must be
-- moved out of that module. This module is the bucket for definitions that have
-- not been specifically moved elsewhere.
module Hasura.RQL.Types.Metadata.Common
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
    Permissions,
    QueryCollections,
    Relationships,
    SchemaRemoteRelationships,
    RemoteSchemaMetadata (..),
    RemoteSchemaPermissionMetadata (..),
    RemoteSchemas,
    RemoteSchemaTypeRelationships (..),
    SetCatalogState (..),
    SourceMetadata (..),
    Sources,
    TableMetadata (..),
    Tables,
    fmComment,
    fmConfiguration,
    fmFunction,
    fmPermissions,
    getSourceName,
    mkSourceMetadata,
    mkTableMeta,
    parseNonSourcesMetadata,
    rsmComment,
    rsmDefinition,
    rsmName,
    rsmPermissions,
    rsmRemoteRelationships,
    rspmComment,
    rspmDefinition,
    rspmRole,
    rstrsName,
    rstrsRelationships,
    smConfiguration,
    smFunctions,
    smKind,
    smName,
    smQueryTags,
    smTables,
    smCustomization,
    tmArrayRelationships,
    tmComputedFields,
    tmConfiguration,
    tmDeletePermissions,
    tmApolloFederationConfig,
    tmEventTriggers,
    tmInsertPermissions,
    tmIsEnum,
    tmObjectRelationships,
    tmRemoteRelationships,
    tmSelectPermissions,
    tmTable,
    tmUpdatePermissions,
    toSourceMetadata,
  )
where

import Control.Lens hiding (set, (.=))
import Data.Aeson.Casing
import Data.Aeson.Extended (FromJSONWithContext (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.HashSet qualified as HS
import Data.List.Extended qualified as L
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Action
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
import Hasura.Session
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

data ComputedFieldMetadata b = ComputedFieldMetadata
  { _cfmName :: ComputedFieldName,
    _cfmDefinition :: ComputedFieldDefinition b,
    _cfmComment :: Comment
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
  { _rspmRole :: RoleName,
    _rspmDefinition :: RemoteSchemaPermissionDefinition,
    _rspmComment :: Maybe Text
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
  { _tmTable :: TableName b,
    _tmIsEnum :: Bool,
    _tmConfiguration :: TableConfig b,
    _tmObjectRelationships :: Relationships (ObjRelDef b),
    _tmArrayRelationships :: Relationships (ArrRelDef b),
    _tmComputedFields :: ComputedFields b,
    _tmRemoteRelationships :: RemoteRelationships,
    _tmInsertPermissions :: Permissions (InsPermDef b),
    _tmSelectPermissions :: Permissions (SelPermDef b),
    _tmUpdatePermissions :: Permissions (UpdPermDef b),
    _tmDeletePermissions :: Permissions (DelPermDef b),
    _tmEventTriggers :: EventTriggers b,
    _tmApolloFederationConfig :: Maybe ApolloFederationConfig
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
    Nothing

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
      <*> o .:? enableAFKey
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
      enableAFKey = "apollo_federation_config"

      getUnexpectedKeys o =
        HS.fromList (KM.keys o) `HS.difference` expectedKeySet

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
            rrKey,
            enableAFKey
          ]

data FunctionMetadata b = FunctionMetadata
  { _fmFunction :: FunctionName b,
    _fmConfiguration :: FunctionConfig,
    _fmPermissions :: [FunctionPermissionInfo],
    _fmComment :: Maybe Text
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
  { _smName :: SourceName,
    _smKind :: BackendSourceKind b,
    _smTables :: Tables b,
    _smFunctions :: Functions b,
    _smConfiguration :: SourceConnConfiguration b,
    _smQueryTags :: Maybe QueryTagsConfig,
    _smCustomization :: SourceCustomization
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
  AB.mkAnyBackend $
    SourceMetadata
      @b
      name
      backendSourceKind
      mempty
      mempty
      config
      Nothing
      customization

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

deriving newtype instance (Semigroup (BackendConfig b)) => Semigroup (BackendConfigWrapper b)

deriving newtype instance (Monoid (BackendConfig b)) => Monoid (BackendConfigWrapper b)

data CatalogStateType
  = CSTCli
  | CSTConsole
  deriving (Show, Eq)

$(deriveJSON defaultOptions {constructorTagModifier = snakeCase . drop 3} ''CatalogStateType)

data SetCatalogState = SetCatalogState
  { _scsType :: CatalogStateType,
    _scsState :: Value
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON ''SetCatalogState)

data CatalogState = CatalogState
  { _csId :: Text,
    _csCliState :: Value,
    _csConsoleState :: Value
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''CatalogState)

data GetCatalogState
  = GetCatalogState
  deriving (Show, Eq)

$(deriveToJSON defaultOptions ''GetCatalogState)

instance FromJSON GetCatalogState where
  parseJSON _ = pure GetCatalogState
