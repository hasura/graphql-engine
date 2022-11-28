{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | In order to avoid circular dependencies while splitting
-- 'Hasura.RQL.Types.Metadata' into multiple modules, some definitions must be
-- moved out of that module. This module is the bucket for definitions that have
-- not been specifically moved elsewhere.
module Hasura.RQL.Types.Metadata.Common
  ( Actions,
    BackendConfigWrapper (..),
    BackendSourceMetadata (..),
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
    RemoteSchemaMetadata,
    RemoteSchemas,
    SetCatalogState (..),
    SourceMetadata (..),
    Sources,
    TableMetadata (..),
    Tables,
    backendSourceMetadataCodec,
    fmComment,
    fmConfiguration,
    fmFunction,
    fmPermissions,
    getSourceName,
    mkSourceMetadata,
    mkTableMeta,
    parseNonSourcesMetadata,
    smConfiguration,
    smFunctions,
    smKind,
    smName,
    smQueryTags,
    smTables,
    smCustomization,
    smHealthCheckConfig,
    sourcesCodec,
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

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Control.Lens hiding (set, (.=))
import Data.Aeson.Casing
import Data.Aeson.Extended (FromJSONWithContext (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec, sortedElemsCodecWith)
import Data.HashSet qualified as HS
import Data.List.Extended qualified as L
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
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
import Hasura.RQL.Types.HealthCheck
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.QueryTags
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.RemoteSchema.Metadata
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.Tag (BackendTag, HasTag (backendTag))
import Hasura.Session

-- | Parse a list of objects into a map from a derived key,
-- failing if the list has duplicates.
parseListAsMap ::
  (Hashable k, T.ToTxt k) =>
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
        "multiple declarations exist for the following "
          <> things
          <> ": "
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

instance Backend b => HasCodec (ComputedFieldMetadata b) where
  codec =
    AC.object (codecNamePrefix @b <> "ComputedFieldMetadata") $
      ComputedFieldMetadata
        <$> requiredField' "name" AC..= _cfmName
        <*> requiredFieldWith' "definition" placeholderCodecViaJSON AC..= _cfmDefinition
        <*> optionalFieldWithOmittedDefault' "comment" Automatic AC..= _cfmComment

instance (Backend b) => ToJSON (ComputedFieldMetadata b) where
  toJSON ComputedFieldMetadata {..} =
    object $
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

type Relationships a = InsOrdHashMap RelName a

type ComputedFields b = InsOrdHashMap ComputedFieldName (ComputedFieldMetadata b)

type RemoteRelationships = InsOrdHashMap RelName RemoteRelationship

type Permissions a = InsOrdHashMap RoleName a

type EventTriggers b = InsOrdHashMap TriggerName (EventTriggerConf b)

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

instance (Backend b) => ToJSON (TableMetadata b) where
  toJSON = genericToJSON hasuraJSON

-- TODO: Replace uses of placeholderCodecViaJSON with proper codecs
instance (Backend b) => HasCodec (TableMetadata b) where
  codec =
    CommentCodec "Representation of a table in metadata, 'tables.yaml' and 'metadata.json'" $
      AC.object (codecNamePrefix @b <> "TableMetadata") $
        TableMetadata
          <$> requiredField' "table" .== _tmTable
          <*> optionalFieldWithOmittedDefault' "is_enum" False .== _tmIsEnum
          <*> optionalFieldWithOmittedDefault "configuration" emptyTableConfig configDoc .== _tmConfiguration
          <*> optSortedList "object_relationships" _rdName .== _tmObjectRelationships
          <*> optSortedList "array_relationships" _rdName .== _tmArrayRelationships
          <*> optSortedList "computed_fields" _cfmName .== _tmComputedFields
          <*> optSortedList "remote_relationships" _rrName .== _tmRemoteRelationships
          <*> optSortedList "insert_permissions" _pdRole .== _tmInsertPermissions
          <*> optSortedList "select_permissions" _pdRole .== _tmSelectPermissions
          <*> optSortedList "update_permissions" _pdRole .== _tmUpdatePermissions
          <*> optSortedList "delete_permissions" _pdRole .== _tmDeletePermissions
          <*> optSortedListViaJSON "event_triggers" etcName .== _tmEventTriggers
          <*> optionalFieldOrNull' "apollo_federation_config" .== _tmApolloFederationConfig
    where
      optSortedListViaJSON ::
        (Eq a, FromJSON a, ToJSON a, Hashable k, Ord k, T.ToTxt k) =>
        Text ->
        (a -> k) ->
        ObjectCodec (InsOrdHashMap k a) (InsOrdHashMap k a)
      optSortedListViaJSON name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodecWith placeholderCodecViaJSON keyForElem) mempty

      optSortedList ::
        (HasCodec a, Eq a, Hashable k, Ord k, T.ToTxt k) =>
        Text ->
        (a -> k) ->
        ObjectCodec (InsOrdHashMap k a) (InsOrdHashMap k a)
      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

      configDoc =
        T.unlines
          [ "Configuration for the table/view",
            "",
            "https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/table-view.html#table-config"
          ]

      (.==) = (AC..=)

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
        "unexpected keys when parsing TableMetadata: "
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

instance (Backend b) => HasCodec (FunctionMetadata b) where
  codec =
    CommentCodec
      ( T.unlines
          [ "A custom SQL function to add to the GraphQL schema with configuration.",
            "",
            "https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#args-syntax"
          ]
      )
      $ AC.object (codecNamePrefix @b <> "FunctionMetadata")
      $ FunctionMetadata
        <$> requiredField "function" nameDoc AC..= _fmFunction
        <*> optionalFieldWithOmittedDefault "configuration" emptyFunctionConfig configDoc AC..= _fmConfiguration
        <*> optionalFieldWithOmittedDefault' "permissions" [] AC..= _fmPermissions
        <*> optionalField' "comment" AC..= _fmComment
    where
      nameDoc = "Name of the SQL function"
      configDoc = "Configuration for the SQL function"

type RemoteSchemaMetadata = RemoteSchemaMetadataG RemoteRelationshipDefinition

type RemoteSchemas = InsOrdHashMap RemoteSchemaName RemoteSchemaMetadata

type Tables b = InsOrdHashMap (TableName b) (TableMetadata b)

type Functions b = InsOrdHashMap (FunctionName b) (FunctionMetadata b)

type Endpoints = InsOrdHashMap EndpointName CreateEndpoint

type Actions = InsOrdHashMap ActionName ActionMetadata

type CronTriggers = InsOrdHashMap TriggerName CronTriggerMetadata

type InheritedRoles = InsOrdHashMap RoleName InheritedRole

-- | Source configuration for a source of backend type @b@ as stored in the Metadata DB.
data SourceMetadata b = SourceMetadata
  { _smName :: SourceName,
    _smKind :: BackendSourceKind b,
    _smTables :: Tables b,
    _smFunctions :: Functions b,
    _smConfiguration :: SourceConnConfiguration b,
    _smQueryTags :: Maybe QueryTagsConfig,
    _smCustomization :: SourceCustomization,
    _smHealthCheckConfig :: Maybe (HealthCheckConfig b)
  }
  deriving (Generic)

$(makeLenses ''SourceMetadata)

deriving instance (Backend b) => Show (SourceMetadata b)

deriving instance (Backend b) => Eq (SourceMetadata b)

instance (Backend b) => FromJSONWithContext (BackendSourceKind b) (SourceMetadata b) where
  parseJSONWithContext _smKind = withObject "Object" $ \o -> do
    _smName <- o .: "name"
    _smTables <- oMapFromL _tmTable <$> o .: "tables"
    _smFunctions <- oMapFromL _fmFunction <$> o .:? "functions" .!= []
    _smConfiguration <- o .: "configuration"
    _smQueryTags <- o .:? "query_tags"
    _smCustomization <- o .:? "customization" .!= emptySourceCustomization
    _smHealthCheckConfig <- o .:? "health_check"
    pure SourceMetadata {..}

backendSourceMetadataCodec :: JSONCodec BackendSourceMetadata
backendSourceMetadataCodec =
  named "SourceMetadata" $
    -- Attempt to match against @SourceMetadata@ codecs for each native backend
    -- type. If none match then apply the @SourceMetadata DataConnector@ codec.
    -- DataConnector is the fallback case because the possible values for its
    -- @_smKind@ property are not statically-known so it is difficult to
    -- unambiguously distinguish a native source value from a dataconnector
    -- source.
    disjointMatchChoicesCodec
      (matcherWithBackendCodec <$> filter (/= DataConnector) supportedBackends) -- list of codecs to try
      (mkCodec (backendTag @('DataConnector))) -- codec for fallback case
  where
    matcherWithBackendCodec :: BackendType -> (BackendSourceMetadata -> Maybe BackendSourceMetadata, JSONCodec BackendSourceMetadata)
    matcherWithBackendCodec backendType =
      (matches backendType, AB.dispatchAnyBackend @Backend (AB.liftTag backendType) mkCodec)

    mkCodec :: forall b. Backend b => (BackendTag b) -> JSONCodec BackendSourceMetadata
    mkCodec _ = anySourceMetadataCodec $ codec @(SourceMetadata b)

    matches :: BackendType -> BackendSourceMetadata -> Maybe BackendSourceMetadata
    matches backendType input =
      if runBackendType input == backendType
        then Just input
        else Nothing

    runBackendType :: BackendSourceMetadata -> BackendType
    runBackendType (BackendSourceMetadata input) = AB.runBackend input \sourceMeta ->
      backendTypeFromBackendSourceKind $ _smKind sourceMeta

anySourceMetadataCodec :: (HasTag b) => JSONCodec (SourceMetadata b) -> JSONCodec BackendSourceMetadata
anySourceMetadataCodec = dimapCodec dec enc
  where
    dec :: HasTag b => SourceMetadata b -> BackendSourceMetadata
    dec = BackendSourceMetadata . AB.mkAnyBackend

    -- This encoding function is partial, but that should be ok.
    enc :: HasTag b => BackendSourceMetadata -> SourceMetadata b
    enc input = fromJust $ AB.unpackAnyBackend $ unBackendSourceMetadata input

instance Backend b => HasCodec (SourceMetadata b) where
  codec =
    AC.object (codecNamePrefix @b <> "SourceMetadata") $
      SourceMetadata
        <$> requiredField' "name" .== _smName
        <*> requiredField' "kind" .== _smKind
        <*> requiredFieldWith' "tables" (sortedElemsCodec _tmTable) .== _smTables
        <*> optionalFieldOrNullWithOmittedDefaultWith' "functions" (sortedElemsCodec _fmFunction) mempty .== _smFunctions
        <*> requiredField' "configuration" .== _smConfiguration
        <*> optionalFieldOrNull' "query_tags" .== _smQueryTags
        <*> optionalFieldWithOmittedDefault' "customization" emptySourceCustomization .== _smCustomization
        <*> healthCheckField
    where
      healthCheckField = case healthCheckImplementation @b of
        Just hci -> optionalFieldOrNullWith' "health_check" (healthCheckConfigCodec hci) .== _smHealthCheckConfig
        Nothing ->
          -- If this backend does not support health check tests then this field
          -- should be excluded from the serialization format.
          pure Nothing

      (.==) = (AC..=)

mkSourceMetadata ::
  forall (b :: BackendType).
  Backend b =>
  SourceName ->
  BackendSourceKind b ->
  SourceConnConfiguration b ->
  SourceCustomization ->
  Maybe (HealthCheckConfig b) ->
  BackendSourceMetadata
mkSourceMetadata name backendSourceKind config customization healthCheckConfig =
  BackendSourceMetadata $
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
        healthCheckConfig

-- | Source configuration as stored in the Metadata DB for some existentialized backend.
newtype BackendSourceMetadata = BackendSourceMetadata {unBackendSourceMetadata :: AB.AnyBackend SourceMetadata}
  deriving newtype (Eq, Show)

toSourceMetadata :: forall b. (Backend b) => Prism' BackendSourceMetadata (SourceMetadata b)
toSourceMetadata = prism' (BackendSourceMetadata . AB.mkAnyBackend) (AB.unpackAnyBackend . unBackendSourceMetadata)

getSourceName :: BackendSourceMetadata -> SourceName
getSourceName e = AB.dispatchAnyBackend @Backend (unBackendSourceMetadata e) _smName

type Sources = InsOrdHashMap SourceName BackendSourceMetadata

sourcesCodec :: AC.JSONCodec Sources
sourcesCodec = sortedElemsCodecWith backendSourceMetadataCodec getSourceName

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
