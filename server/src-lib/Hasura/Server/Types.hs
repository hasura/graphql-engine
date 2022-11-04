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
    askMetadataDefaults,
    getRequestId,
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

data ServerConfigCtx = ServerConfigCtx
  { _sccFunctionPermsCtx :: Options.InferFunctionPermissions,
    _sccRemoteSchemaPermsCtx :: Options.RemoteSchemaPermissions,
    _sccSQLGenCtx :: SQLGenCtx,
    _sccMaintenanceMode :: MaintenanceMode (),
    _sccExperimentalFeatures :: Set.HashSet ExperimentalFeature,
    _sccEventingMode :: EventingMode,
    _sccReadOnlyMode :: ReadOnlyMode,
    -- | stores global default naming convention
    _sccDefaultNamingConvention :: Maybe NamingCase,
    _sccMetadataDefaults :: MetadataDefaults
  }
  deriving (Show, Eq)

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
