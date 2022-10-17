{-# LANGUAGE MagicHash #-}

module Hasura.Server.Types
  ( ExperimentalFeature (..),
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
    getRequestId,
    ServerReplicas,
    -- exported only for tests
    unsafeServerReplicas,
    oneServerReplica,
    getServerReplicasInt,
    safeServerReplicas,
    ResizePoolStrategy (..),
  )
where

import Data.Aeson
import Data.HashSet qualified as Set
import Database.PG.Query qualified as PG
import GHC.Exts (Int (I#), Word (W#), int2Word#)
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Prelude
import Hasura.RQL.Types.Common
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
  deriving (Show, Eq, Generic)

instance Hashable ExperimentalFeature

instance FromJSON ExperimentalFeature where
  parseJSON = withText "ExperimentalFeature" $ \case
    "inherited_roles" -> pure EFInheritedRoles
    "optimize_permission_filters" -> pure EFOptimizePermissionFilters
    "naming_convention" -> pure EFNamingConventions
    "streaming_subscriptions" -> pure EFStreamingSubscriptions
    "hide_update_many_fields" -> pure EFHideUpdateManyFields
    "apollo_federation" -> pure EFApolloFederation
    "bigquery_string_numeric_input" -> pure EFBigQueryStringNumericInput
    _ -> fail "ExperimentalFeature can only be one of these value: inherited_roles, optimize_permission_filters, hide_update_many_fields, naming_convention, streaming_subscriptions apollo_federation, or bigquery_string_numeric_input"

instance ToJSON ExperimentalFeature where
  toJSON = \case
    EFInheritedRoles -> "inherited_roles"
    EFOptimizePermissionFilters -> "optimize_permission_filters"
    EFNamingConventions -> "naming_convention"
    EFStreamingSubscriptions -> "streaming_subscriptions"
    EFApolloFederation -> "apollo_federation"
    EFHideUpdateManyFields -> "hide_update_many_fields"
    EFBigQueryStringNumericInput -> "bigquery_string_numeric_input"

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
    _sccDefaultNamingConvention :: Maybe NamingCase
  }
  deriving (Show, Eq)

class (Monad m) => HasServerConfigCtx m where
  askServerConfigCtx :: m ServerConfigCtx

instance HasServerConfigCtx m => HasServerConfigCtx (ReaderT r m) where
  askServerConfigCtx = lift askServerConfigCtx

instance HasServerConfigCtx m => HasServerConfigCtx (ExceptT e m) where
  askServerConfigCtx = lift askServerConfigCtx

instance HasServerConfigCtx m => HasServerConfigCtx (StateT s m) where
  askServerConfigCtx = lift askServerConfigCtx

-- | Number of server instances. A wrapper over @'Word' type, a non-negative integer
-- with the same size as @'Int'.
newtype ServerReplicas = ServerReplicas {serverReplicaNumber :: Word}
  deriving (Show, Eq)

unsafeServerReplicas :: Word -> ServerReplicas
unsafeServerReplicas = ServerReplicas

oneServerReplica :: ServerReplicas
oneServerReplica = ServerReplicas 1

-- | Safely build @'ServerReplicas' from non-negative and non-zero @'Int' value.
safeServerReplicas :: Int -> Either Text ServerReplicas
safeServerReplicas i@(I# i#)
  | i <= 0 = Left $ "Expecting a non-zero and non-negative integer for ServerReplicas but got " <> tshow i
  | otherwise = Right $ ServerReplicas $ W# (int2Word# i#)

-- | Get server replic count in @'Int'
getServerReplicasInt :: ServerReplicas -> Int
getServerReplicasInt (ServerReplicas replicaNumber) = fromIntegral replicaNumber

-- | A strategy for resizing a pool
data ResizePoolStrategy
  = -- | Never resize the pool
    NeverResizePool
  | -- | Resize the pool by using provided total maximum connections setting value
    ResizePool Int
