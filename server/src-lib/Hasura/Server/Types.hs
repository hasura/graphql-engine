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
    ServerConfigCtx (..),
    HasServerConfigCtx (..),
    CheckFeatureFlag (..),
    askMetadataDefaults,
    getRequestId,
    ApolloFederationStatus (..),
    isApolloFederationEnabled,
  )
where

import Data.Aeson
import Data.HashSet qualified as Set
import Data.Text (intercalate, unpack)
import Database.PG.Query qualified as PG
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Prelude hiding (intercalate)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata (MetadataDefaults)
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

instance Hashable ExperimentalFeature

instance FromJSON ExperimentalFeature where
  parseJSON = withText "ExperimentalFeature" $ \case
    k | Just (_, ef) <- find ((== k) . fst) experimentalFeatures -> return $ ef
    _ ->
      fail $
        "ExperimentalFeature can only be one of these values: "
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
    withBool "MaintenanceMode" $
      pure . bool MaintenanceModeDisabled (MaintenanceModeEnabled ())

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

-- | This type represents an aggregate of different configuration options used
-- throughout the engine. The fields are the union of a subset of 'AppEnv' and a
-- subset of 'AppContext'.
--
-- This type should be considered as deprecated: avoid using it directly when
-- you can use 'AppEnv' or 'AppContext', and avoid using the entirety of it when
-- you only need a subset of the fields. Also avoid adding new fields if
-- possible, but if you do so, make sure to adjust the @Eq@ instance
-- accordingly.
data ServerConfigCtx = ServerConfigCtx
  { _sccFunctionPermsCtx :: Options.InferFunctionPermissions,
    _sccRemoteSchemaPermsCtx :: Options.RemoteSchemaPermissions,
    _sccSQLGenCtx :: SQLGenCtx,
    _sccMaintenanceMode :: MaintenanceMode (),
    _sccExperimentalFeatures :: Set.HashSet ExperimentalFeature,
    _sccEventingMode :: EventingMode,
    _sccReadOnlyMode :: ReadOnlyMode,
    -- | stores global default naming convention
    _sccDefaultNamingConvention :: NamingCase,
    _sccMetadataDefaults :: MetadataDefaults,
    _sccCheckFeatureFlag :: CheckFeatureFlag,
    _sccApolloFederationStatus :: ApolloFederationStatus
  }

-- We are currently using the entire 'ServerConfigCtx' as an input to the schema
-- cache build, and it therefore requires an 'Eq' instance. However, only a few
-- fields will change over time: those coming from the 'AppContext', and not
-- those coming from the 'AppEnv'. Consequently, this instance only checks the
-- relevant fields.
--
-- The way to fix this will be to use a smaller type as the input to the schema
-- build, such as 'AppContext' (or, rather, a relevant subset), on which a
-- "correct" @Eq@ instance can be defined.
instance Eq ServerConfigCtx where
  (==) = (==) `on` extractDynamicFields
    where
      extractDynamicFields ServerConfigCtx {..} =
        ( _sccFunctionPermsCtx,
          _sccRemoteSchemaPermsCtx,
          _sccSQLGenCtx,
          _sccExperimentalFeatures,
          _sccDefaultNamingConvention,
          _sccMetadataDefaults,
          _sccApolloFederationStatus
        )

askMetadataDefaults :: HasServerConfigCtx m => m MetadataDefaults
askMetadataDefaults = do
  ServerConfigCtx {_sccMetadataDefaults} <- askServerConfigCtx
  pure _sccMetadataDefaults

class (Monad m) => HasServerConfigCtx m where
  askServerConfigCtx :: m ServerConfigCtx

instance HasServerConfigCtx m => HasServerConfigCtx (ReaderT r m) where
  askServerConfigCtx = lift askServerConfigCtx

instance HasServerConfigCtx m => HasServerConfigCtx (ExceptT e m) where
  askServerConfigCtx = lift askServerConfigCtx

instance HasServerConfigCtx m => HasServerConfigCtx (StateT s m) where
  askServerConfigCtx = lift askServerConfigCtx

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
