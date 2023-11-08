{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.Types
  ( ExperimentalFeature (..),
    experimentalFeatureKey,
    InstanceId (..),
    generateInstanceId,
    MetadataDbId (..),
    DbUid (..),
    mdDbIdToDbUid,
    MaintenanceMode (..),
    EventingMode (..),
    ReadOnlyMode (..),
    DbVersion (DbVersion),
    PGVersion (PGVersion),
    pgToDbVersion,
    RequestId (..),
    CheckFeatureFlag (..),
    getRequestId,
    ApolloFederationStatus (..),
    TriggersErrorLogLevelStatus (..),
    isApolloFederationEnabled,
    isTriggersErrorLogLevelEnabled,
    ModelInfoLogState (..),
    GranularPrometheusMetricsState (..),
    OpenTelemetryExporterState (..),
    CloseWebsocketsOnMetadataChangeStatus (..),
    isCloseWebsocketsOnMetadataChangeStatusEnabled,
    PersistedQueriesState (..),
    PersistedQueryRequest (..),
    ExtPersistedQueryRequest (..),
    ExtQueryReqs (..),
    MonadGetPolicies (..),
  )
where

import Control.Lens qualified as Lens
import Data.Aeson hiding (json)
import Data.Aeson.Casing qualified as J
import Data.Aeson.Lens
import Data.Aeson.TH qualified as J
import Data.Text (intercalate, unpack)
import Database.PG.Query qualified as PG
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.Prelude hiding (intercalate)
import Hasura.RQL.Types.ApiLimit
import Hasura.Server.Init.FeatureFlag (CheckFeatureFlag (..))
import Hasura.Server.Utils
import Network.HTTP.Types qualified as HTTP

newtype RequestId = RequestId {unRequestId :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, Hashable)

getRequestId :: (MonadIO m) => [HTTP.Header] -> m (RequestId, [HTTP.Header])
getRequestId headers = do
  -- generate a request id for every request if the client has not sent it
  let mkHeader = (requestIdHeader,)
  case getRequestHeader requestIdHeader headers of
    Nothing -> do
      reqId <- liftIO generateFingerprint
      let r = RequestId reqId
      pure (r, mkHeader (txtToBs reqId) : headers)
    Just reqId -> pure (RequestId $ bsToTxt reqId, headers)

-- | A uuid of a source database.
newtype DbUid = DbUid {getDbUid :: Text}
  deriving (Show, Eq, ToJSON, FromJSON)

newtype DbVersion = DbVersion {unDbVersion :: Text}
  deriving (Show, Eq, ToJSON)

newtype PGVersion = PGVersion {unPGVersion :: Int}
  deriving (Show, Eq, ToJSON)

pgToDbVersion :: PGVersion -> DbVersion
pgToDbVersion = DbVersion . tshow . unPGVersion

-- | A uuid of the postgres metadata db.
newtype MetadataDbId = MetadataDbId {getMetadataDbId :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, PG.FromCol, PG.ToPrepArg)

mdDbIdToDbUid :: MetadataDbId -> DbUid
mdDbIdToDbUid = DbUid . getMetadataDbId

-- | A UUID for each running instance of graphql-engine, generated fresh each
-- time graphql-engine starts up
newtype InstanceId = InstanceId {getInstanceId :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, PG.FromCol, PG.ToPrepArg)

-- | Generate an 'InstanceId' from a 'UUID'
generateInstanceId :: IO InstanceId
generateInstanceId = InstanceId <$> generateFingerprint

data ExperimentalFeature
  = EFInheritedRoles
  | EFOptimizePermissionFilters
  | EFNamingConventions
  | EFStreamingSubscriptions
  | EFApolloFederation
  | EFHideUpdateManyFields
  | EFBigQueryStringNumericInput
  | EFHideAggregationPredicates
  | EFHideStreamFields
  | EFGroupByAggregations
  | EFDisablePostgresArrays
  deriving (Bounded, Enum, Eq, Generic, Show)

experimentalFeatureKey :: ExperimentalFeature -> Text
experimentalFeatureKey = \case
  EFInheritedRoles -> "inherited_roles"
  EFOptimizePermissionFilters -> "optimize_permission_filters"
  EFNamingConventions -> "naming_convention"
  EFStreamingSubscriptions -> "streaming_subscriptions"
  EFApolloFederation -> "apollo_federation"
  EFHideUpdateManyFields -> "hide_update_many_fields"
  EFBigQueryStringNumericInput -> "bigquery_string_numeric_input"
  EFHideAggregationPredicates -> "hide_aggregation_predicates"
  EFHideStreamFields -> "hide_stream_fields"
  EFGroupByAggregations -> "group_by_aggregations"
  EFDisablePostgresArrays -> "disable_postgres_arrays"

instance Hashable ExperimentalFeature

instance FromJSON ExperimentalFeature where
  parseJSON = withText "ExperimentalFeature" $ \case
    k | Just (_, ef) <- find ((== k) . fst) experimentalFeatures -> return $ ef
    _ ->
      fail
        $ "ExperimentalFeature can only be one of these values: "
        <> unpack (intercalate "," (map fst experimentalFeatures))
    where
      experimentalFeatures :: [(Text, ExperimentalFeature)]
      experimentalFeatures =
        [ (experimentalFeatureKey ef, ef)
          | ef <- [minBound .. maxBound]
        ]

instance ToJSON ExperimentalFeature where
  toJSON = toJSON . experimentalFeatureKey

data MaintenanceMode a = MaintenanceModeEnabled a | MaintenanceModeDisabled
  deriving (Show, Eq)

instance FromJSON (MaintenanceMode ()) where
  parseJSON =
    withBool "MaintenanceMode"
      $ pure
      . bool MaintenanceModeDisabled (MaintenanceModeEnabled ())

instance ToJSON (MaintenanceMode ()) where
  toJSON = Bool . (== MaintenanceModeEnabled ())

-- | See Note [ReadOnly Mode]
data ReadOnlyMode = ReadOnlyModeEnabled | ReadOnlyModeDisabled
  deriving (Show, Eq)

-- | EventingMode decides whether the eventing subsystem should be enabled or disabled.
-- `EventDisabled` mode disables Event Triggers, Async Actions, Scheduled Events and source catalaog migrations.
-- This is an internal feature and will not be exposed to users.
data EventingMode = EventingEnabled | EventingDisabled
  deriving (Show, Eq)

-- | Whether or not to enable apollo federation fields.
data ApolloFederationStatus = ApolloFederationEnabled | ApolloFederationDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance NFData ApolloFederationStatus

instance Hashable ApolloFederationStatus

instance FromJSON ApolloFederationStatus where
  parseJSON = fmap (bool ApolloFederationDisabled ApolloFederationEnabled) . parseJSON

isApolloFederationEnabled :: ApolloFederationStatus -> Bool
isApolloFederationEnabled = \case
  ApolloFederationEnabled -> True
  ApolloFederationDisabled -> False

instance ToJSON ApolloFederationStatus where
  toJSON = toJSON . isApolloFederationEnabled

data TriggersErrorLogLevelStatus = TriggersErrorLogLevelEnabled | TriggersErrorLogLevelDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TriggersErrorLogLevelStatus where
  parseJSON = fmap (bool TriggersErrorLogLevelDisabled TriggersErrorLogLevelEnabled) . parseJSON

isTriggersErrorLogLevelEnabled :: TriggersErrorLogLevelStatus -> Bool
isTriggersErrorLogLevelEnabled = \case
  TriggersErrorLogLevelEnabled -> True
  TriggersErrorLogLevelDisabled -> False

instance ToJSON TriggersErrorLogLevelStatus where
  toJSON = toJSON . isTriggersErrorLogLevelEnabled

data ModelInfoLogState
  = ModelInfoLogOff
  | ModelInfoLogOn
  deriving (Eq, Show)

instance FromJSON ModelInfoLogState where
  parseJSON = withBool "ModelInfoLogState" $ \case
    False -> pure ModelInfoLogOff
    True -> pure ModelInfoLogOn

instance ToJSON ModelInfoLogState where
  toJSON = \case
    ModelInfoLogOff -> Bool False
    ModelInfoLogOn -> Bool True

-- | Whether or not to enable granular metrics for Prometheus.
--
-- `GranularMetricsOn` will enable the dynamic labels for the metrics.
-- `GranularMetricsOff` will disable the dynamic labels for the metrics.
--
-- **Warning**: Enabling dynamic labels for Prometheus metrics can cause cardinality
-- issues and can cause memory usage to increase.
data GranularPrometheusMetricsState
  = GranularMetricsOff
  | GranularMetricsOn
  deriving (Eq, Show)

instance FromJSON GranularPrometheusMetricsState where
  parseJSON = withBool "GranularPrometheusMetricsState" $ \case
    False -> pure GranularMetricsOff
    True -> pure GranularMetricsOn

instance ToJSON GranularPrometheusMetricsState where
  toJSON = \case
    GranularMetricsOff -> Bool False
    GranularMetricsOn -> Bool True

-- | Whether or not to enable OpenTelemetry Exporter.
--
-- `OpenTelemetryExporterOn` will enable exporting of traces & metrics via the OTel Exporter.
-- `OpenTelemetryExporterOff` will disable exporting of traces & metrics via the OTel Exporter.
data OpenTelemetryExporterState
  = OpenTelemetryExporterOff
  | OpenTelemetryExporterOn
  deriving (Eq, Show)

instance FromJSON OpenTelemetryExporterState where
  parseJSON = withBool "OpenTelemetryExporterState" $ \case
    False -> pure OpenTelemetryExporterOff
    True -> pure OpenTelemetryExporterOn

instance ToJSON OpenTelemetryExporterState where
  toJSON = \case
    OpenTelemetryExporterOff -> Bool False
    OpenTelemetryExporterOn -> Bool True

-- | Whether or not to close websocket connections on metadata change.
data CloseWebsocketsOnMetadataChangeStatus = CWMCEnabled | CWMCDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance NFData CloseWebsocketsOnMetadataChangeStatus

instance Hashable CloseWebsocketsOnMetadataChangeStatus

instance FromJSON CloseWebsocketsOnMetadataChangeStatus where
  parseJSON = fmap (bool CWMCDisabled CWMCEnabled) . parseJSON

isCloseWebsocketsOnMetadataChangeStatusEnabled :: CloseWebsocketsOnMetadataChangeStatus -> Bool
isCloseWebsocketsOnMetadataChangeStatusEnabled = \case
  CWMCEnabled -> True
  CWMCDisabled -> False

instance ToJSON CloseWebsocketsOnMetadataChangeStatus where
  toJSON = toJSON . isCloseWebsocketsOnMetadataChangeStatusEnabled

data PersistedQueriesState
  = PersistedQueriesDisabled
  | PersistedQueriesEnabled
  deriving (Eq, Show)

instance FromJSON PersistedQueriesState where
  parseJSON = withBool "PersistedQueriesState" $ \case
    False -> pure PersistedQueriesDisabled
    True -> pure PersistedQueriesEnabled

instance ToJSON PersistedQueriesState where
  toJSON = \case
    PersistedQueriesDisabled -> Bool False
    PersistedQueriesEnabled -> Bool True

-- | The persisted query request sent by Apollo clients. Ref:
-- https://www.apollographql.com/docs/apollo-server/performance/apq/#verify
data PersistedQueryRequest = PersistedQueryRequest
  { _pqrVersion :: Int,
    _pqrSha256Hash :: Text
  }
  deriving (Show, Eq, Generic)

$(J.deriveToJSON (J.aesonPrefix J.camelCase) {J.omitNothingFields = True} ''PersistedQueryRequest)

instance FromJSON PersistedQueryRequest where
  parseJSON = withObject "PersistedQueryRequest" $ \o -> do
    q <- o .: "persistedQuery"
    version <- q .: "version"
    hash <- q .: "sha256Hash"
    pure $ PersistedQueryRequest version hash

-- | The persisted query request sent in the POST body by Apollo Clients. Ref:
-- <https://github.com/apollographql/apollo-link-persisted-queries#protocol>
-- Read as Extended Persisted Query request.
-- The Query field in the request body, will contain the query only when it is not present in the system,
-- thus `HGE.GQLReq (Maybe a)`
data ExtPersistedQueryRequest a = ExtPersistedQueryRequest
  { _extOperationName :: (Maybe GH.OperationName),
    _extQuery :: (Maybe GH.GQLQueryText),
    _extVariables :: (Maybe GH.VariableValues),
    _extExtensions :: PersistedQueryRequest
  }
  deriving (Show, Eq, Generic)

$(J.deriveJSON (J.aesonDrop 4 J.camelCase) {J.omitNothingFields = True} ''ExtPersistedQueryRequest)

-- | The POST request might either be a normal GQL request or a persisted query request
data ExtQueryReqs
  = EqrGQLReq GH.ReqsText
  | EqrAPQReq (ExtPersistedQueryRequest (GH.GQLReq GH.GQLQueryText))
  deriving (Show, Eq, Generic)

instance ToJSON ExtQueryReqs where
  toJSON (EqrGQLReq qs) = toJSON qs
  toJSON (EqrAPQReq q) = toJSON q

instance FromJSON ExtQueryReqs where
  parseJSON arr@Array {} = EqrGQLReq <$> (GH.GQLBatchedReqs <$> parseJSON arr)
  parseJSON json
    -- The APQ requests, have a special key called as Extensions
    | isJust (json Lens.^? key "extensions") = EqrAPQReq <$> parseJSON json
    | otherwise = EqrGQLReq <$> (GH.GQLSingleRequest <$> parseJSON json)

class (Monad m) => MonadGetPolicies m where
  runGetApiTimeLimit ::
    m (Maybe MaxTime)

  -- 'GranularPrometheusMetricsState' is used to decide if dynamic labels needs to be
  -- added when emitting the prometheus metric. The state of this can be dynamically
  -- changed via policies. Hence we need to fetch the value from the policy everytime
  -- before emitting the metric. Thus we create an IO action which fetches the value.
  runGetPrometheusMetricsGranularity ::
    m (IO GranularPrometheusMetricsState)

  runGetModelInfoLogStatus ::
    m (IO ModelInfoLogState)

instance (MonadGetPolicies m) => MonadGetPolicies (ReaderT r m) where
  runGetApiTimeLimit = lift runGetApiTimeLimit
  runGetPrometheusMetricsGranularity = lift runGetPrometheusMetricsGranularity
  runGetModelInfoLogStatus = lift $ runGetModelInfoLogStatus

instance (MonadGetPolicies m) => MonadGetPolicies (ExceptT e m) where
  runGetApiTimeLimit = lift runGetApiTimeLimit
  runGetPrometheusMetricsGranularity = lift runGetPrometheusMetricsGranularity
  runGetModelInfoLogStatus = lift $ runGetModelInfoLogStatus

instance (MonadGetPolicies m) => MonadGetPolicies (StateT w m) where
  runGetApiTimeLimit = lift runGetApiTimeLimit
  runGetPrometheusMetricsGranularity = lift runGetPrometheusMetricsGranularity
  runGetModelInfoLogStatus = lift $ runGetModelInfoLogStatus
