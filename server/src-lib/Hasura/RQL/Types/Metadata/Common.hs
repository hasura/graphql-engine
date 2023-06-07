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
    CronTriggers,
    LogicalModels,
    Endpoints,
    NativeQueries,
    StoredProcedures,
    EventTriggers,
    Functions,
    GetCatalogState (..),
    InheritedRoles,
    QueryCollections,
    RemoteSchemaMetadata,
    RemoteSchemas,
    SetCatalogState (..),
    SourceMetadata (..),
    Sources,
    Tables,
    backendSourceMetadataCodec,
    getSourceName,
    mkSourceMetadata,
    parseNonSourcesMetadata,
    smConfiguration,
    smFunctions,
    smKind,
    smName,
    smQueryTags,
    smTables,
    smCustomization,
    smNativeQueries,
    smStoredProcedures,
    smLogicalModels,
    smHealthCheckConfig,
    sourcesCodec,
    toSourceMetadata,
  )
where

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Control.Lens hiding (set, (.=))
import Data.Aeson.Casing
import Data.Aeson.Extended (FromJSONWithContext (..))
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec, sortedElemsCodecWith)
import Data.List.Extended qualified as L
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Function.Metadata (FunctionMetadata (..))
import Hasura.LogicalModel.Metadata (LogicalModelMetadata (..), LogicalModelName)
import Hasura.NativeQuery.Metadata (NativeQueryMetadata (..), NativeQueryName)
import Hasura.Prelude
import Hasura.QueryTags.Types
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (BackendTag, HasTag (backendTag), backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.HealthCheck
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SourceCustomization
import Hasura.RemoteSchema.Metadata
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.StoredProcedure.Metadata (StoredProcedureMetadata (..))
import Hasura.Table.Metadata

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
  unless (null duplicates)
    $ fail
    $ T.unpack
    $ "multiple declarations exist for the following "
    <> things
    <> ": "
    <> T.commaSeparated duplicates
  pure $ oMapFromL mapFn list

type EventTriggers b = InsOrdHashMap TriggerName (EventTriggerConf b)

type RemoteSchemaMetadata = RemoteSchemaMetadataG RemoteRelationshipDefinition

type RemoteSchemas = InsOrdHashMap RemoteSchemaName RemoteSchemaMetadata

type Tables b = InsOrdHashMap (TableName b) (TableMetadata b)

type Functions b = InsOrdHashMap (FunctionName b) (FunctionMetadata b)

type NativeQueries b = InsOrdHashMap NativeQueryName (NativeQueryMetadata b)

type StoredProcedures b = InsOrdHashMap (FunctionName b) (StoredProcedureMetadata b)

type LogicalModels b = InsOrdHashMap LogicalModelName (LogicalModelMetadata b)

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
    _smNativeQueries :: NativeQueries b,
    _smStoredProcedures :: StoredProcedures b,
    _smLogicalModels :: LogicalModels b,
    _smConfiguration :: SourceConnConfiguration b,
    _smQueryTags :: Maybe QueryTagsConfig,
    _smCustomization :: SourceCustomization,
    -- | https://hasura.io/docs/latest/deployment/health-checks/source-health-check/
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
    _smNativeQueries <- oMapFromL _nqmRootFieldName <$> o .:? "native_queries" .!= []
    _smStoredProcedures <- oMapFromL _spmStoredProcedure <$> o .:? "stored_procedures" .!= []
    _smLogicalModels <- oMapFromL _lmmName <$> o .:? "logical_models" .!= []
    _smConfiguration <- o .: "configuration"
    _smQueryTags <- o .:? "query_tags"
    _smCustomization <- o .:? "customization" .!= emptySourceCustomization
    _smHealthCheckConfig <- o .:? "health_check"
    pure SourceMetadata {..}

backendSourceMetadataCodec :: JSONCodec BackendSourceMetadata
backendSourceMetadataCodec =
  named "SourceMetadata"
    $
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

    mkCodec :: forall b. (Backend b) => (BackendTag b) -> JSONCodec BackendSourceMetadata
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
    dec :: (HasTag b) => SourceMetadata b -> BackendSourceMetadata
    dec = BackendSourceMetadata . AB.mkAnyBackend

    -- This encoding function is partial, but that should be ok.
    enc :: (HasTag b) => BackendSourceMetadata -> SourceMetadata b
    enc input = fromJust $ AB.unpackAnyBackend $ unBackendSourceMetadata input

instance (Backend b) => HasCodec (SourceMetadata b) where
  codec =
    AC.object (backendPrefix @b <> "SourceMetadata")
      $ SourceMetadata
      <$> requiredField' "name"
      .== _smName
        <*> requiredField' "kind"
      .== _smKind
        <*> requiredFieldWith' "tables" (sortedElemsCodec _tmTable)
      .== _smTables
        <*> optionalFieldOrNullWithOmittedDefaultWith' "functions" (sortedElemsCodec _fmFunction) mempty
      .== _smFunctions
        <*> optionalFieldOrNullWithOmittedDefaultWith' "native_queries" (sortedElemsCodec _nqmRootFieldName) mempty
      .== _smNativeQueries
        <*> optionalFieldOrNullWithOmittedDefaultWith' "stored_procedures" (sortedElemsCodec _spmStoredProcedure) mempty
      .== _smStoredProcedures
        <*> optionalFieldOrNullWithOmittedDefaultWith' "logical_models" (sortedElemsCodec _lmmName) mempty
      .== _smLogicalModels
        <*> requiredField' "configuration"
      .== _smConfiguration
        <*> optionalFieldOrNull' "query_tags"
      .== _smQueryTags
        <*> optionalFieldWithOmittedDefault' "customization" emptySourceCustomization
      .== _smCustomization
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
  (Backend b) =>
  SourceName ->
  BackendSourceKind b ->
  SourceConnConfiguration b ->
  SourceCustomization ->
  Maybe (HealthCheckConfig b) ->
  BackendSourceMetadata
mkSourceMetadata name backendSourceKind config customization healthCheckConfig =
  BackendSourceMetadata
    $ AB.mkAnyBackend
    $ SourceMetadata
      @b
      name
      backendSourceKind
      mempty
      mempty
      mempty
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
    parseListAsMap "remote schemas" _rsmName
      $ o
      .:? "remote_schemas"
      .!= []
  queryCollections <-
    parseListAsMap "query collections" _ccName
      $ o
      .:? "query_collections"
      .!= []
  allowlist <- parseListAsMap "allowlist entries" aeCollection $ o .:? "allowlist" .!= []
  customTypes <- o .:? "custom_types" .!= emptyCustomTypes
  actions <- parseListAsMap "actions" _amName $ o .:? "actions" .!= []
  cronTriggers <-
    parseListAsMap "cron triggers" ctName
      $ o
      .:? "cron_triggers"
      .!= []

  apiLimits <- o .:? "api_limits" .!= emptyApiLimit
  metricsConfig <- o .:? "metrics_config" .!= emptyMetricsConfig
  inheritedRoles <-
    parseListAsMap "inherited roles" _rRoleName
      $ o
      .:? "inherited_roles"
      .!= []
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

instance (Backend b) => HasCodec (BackendConfigWrapper b) where
  codec = dimapCodec BackendConfigWrapper unBackendConfigWrapper codec

data CatalogStateType
  = CSTCli
  | CSTConsole
  deriving stock (Show, Eq, Generic)

instance FromJSON CatalogStateType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = snakeCase . drop 3}

instance ToJSON CatalogStateType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = snakeCase . drop 3}
  toEncoding = genericToEncoding defaultOptions {constructorTagModifier = snakeCase . drop 3}

data SetCatalogState = SetCatalogState
  { _scsType :: CatalogStateType,
    _scsState :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SetCatalogState where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON SetCatalogState where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data CatalogState = CatalogState
  { _csId :: Text,
    _csCliState :: Value,
    _csConsoleState :: Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CatalogState where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data GetCatalogState
  = GetCatalogState
  deriving stock (Show, Eq, Generic)

instance ToJSON GetCatalogState where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GetCatalogState where
  parseJSON _ = pure GetCatalogState
