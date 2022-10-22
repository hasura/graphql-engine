module Hasura.Server.Types
  ( ExperimentalFeature (..),
    InstanceId (..),
    MaintenanceMode (..),
    EventingMode (..),
    ReadOnlyMode (..),
    PGVersion (PGVersion),
    RequestId (..),
    ServerConfigCtx (..),
    StreamingSubscriptionsCtx (..),
    HasServerConfigCtx (..),
    getRequestId,
  )
where

import Data.Aeson
import Data.HashSet qualified as Set
import Database.PG.Query qualified as Q
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SourceCustomization (NamingCase)
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

newtype DbUid = DbUid {getDbUid :: Text}
  deriving (Show, Eq, ToJSON, FromJSON)

newtype PGVersion = PGVersion {unPGVersion :: Int}
  deriving (Show, Eq, ToJSON)

newtype InstanceId = InstanceId {getInstanceId :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, Q.FromCol, Q.ToPrepArg)

data ExperimentalFeature
  = EFInheritedRoles
  | EFOptimizePermissionFilters
  | EFNamingConventions
  | EFStreamingSubscriptions
  deriving (Show, Eq, Generic)

instance Hashable ExperimentalFeature

instance FromJSON ExperimentalFeature where
  parseJSON = withText "ExperimentalFeature" $ \case
    "inherited_roles" -> pure EFInheritedRoles
    "optimize_permission_filters" -> pure EFOptimizePermissionFilters
    "naming_convention" -> pure EFNamingConventions
    "streaming_subscriptions" -> pure EFStreamingSubscriptions
    _ -> fail "ExperimentalFeature can only be one of these value: inherited_roles, optimize_permission_filters, naming_convention or streaming_subscriptions"

instance ToJSON ExperimentalFeature where
  toJSON = \case
    EFInheritedRoles -> "inherited_roles"
    EFOptimizePermissionFilters -> "optimize_permission_filters"
    EFNamingConventions -> "naming_convention"
    EFStreamingSubscriptions -> "streaming_subscriptions"

data MaintenanceMode a = MaintenanceModeEnabled a | MaintenanceModeDisabled
  deriving (Show, Eq)

instance FromJSON (MaintenanceMode ()) where
  parseJSON =
    withBool "MaintenanceMode" $
      pure . bool MaintenanceModeDisabled (MaintenanceModeEnabled ())

instance ToJSON (MaintenanceMode ()) where
  toJSON = Bool . (== MaintenanceModeEnabled ())

data StreamingSubscriptionsCtx = StreamingSubscriptionsEnabled | StreamingSubscriptionsDisabled
  deriving (Show, Eq)

-- | See Note [ReadOnly Mode]
data ReadOnlyMode = ReadOnlyModeEnabled | ReadOnlyModeDisabled
  deriving (Show, Eq)

-- | EventingMode decides whether the eventing subsystem should be enabled or disabled.
-- `EventDisabled` mode disables Event Triggers, Async Actions, Scheduled Events and source catalaog migrations.
-- This is an internal feature and will not be exposed to users.
data EventingMode = EventingEnabled | EventingDisabled
  deriving (Show, Eq)

data ServerConfigCtx = ServerConfigCtx
  { _sccFunctionPermsCtx :: FunctionPermissionsCtx,
    _sccRemoteSchemaPermsCtx :: RemoteSchemaPermsCtx,
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
