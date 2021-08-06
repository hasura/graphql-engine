module Hasura.Server.Types where

import           Hasura.Prelude

import           Data.Aeson

import qualified Data.HashSet                  as Set
import qualified Database.PG.Query             as Q
import qualified Network.HTTP.Types            as HTTP

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.Server.Utils

newtype RequestId
  = RequestId { unRequestId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON, Hashable)

getRequestId :: (MonadIO m) => [HTTP.Header] -> m (RequestId, [HTTP.Header])
getRequestId headers = do
  -- generate a request id for every request if the client has not sent it
  let mkHeader = (requestIdHeader,)
  case getRequestHeader requestIdHeader headers  of
    Nothing    -> do
      reqId <- liftIO generateFingerprint
      let r = RequestId reqId
      pure (r, mkHeader (txtToBs reqId) : headers)
    Just reqId -> pure (RequestId $ bsToTxt reqId, headers)

newtype DbUid
  = DbUid { getDbUid :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype PGVersion
  = PGVersion { unPGVersion :: Int }
  deriving (Show, Eq, ToJSON)

newtype InstanceId
  = InstanceId { getInstanceId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON, Q.FromCol, Q.ToPrepArg)

data ExperimentalFeature
  = EFInheritedRoles
  deriving (Show, Eq, Generic)

instance Hashable ExperimentalFeature

-- TODO: when there are more than one constuctors in `ExperimentalFeature`, we should
-- auto derive the JSON instances. Doing it with a single data constructor messes
-- up the JSON instances which is why it's manually implemented here

instance FromJSON ExperimentalFeature where
  parseJSON = withText "ExperimentalFeature" $ \case
    "inherited_roles" -> pure EFInheritedRoles
    _                 -> fail "ExperimentalFeature can only be one of these value: inherited_roles "

instance ToJSON ExperimentalFeature where
  toJSON = \case
    EFInheritedRoles -> "inherited_roles"

data MaintenanceMode = MaintenanceModeEnabled | MaintenanceModeDisabled
  deriving (Show, Eq)

instance FromJSON MaintenanceMode where
  parseJSON = withBool "MaintenanceMode" $
    pure . bool MaintenanceModeDisabled MaintenanceModeEnabled

instance ToJSON MaintenanceMode where
  toJSON = Bool . (== MaintenanceModeEnabled)

data ServerConfigCtx
  = ServerConfigCtx
  { _sccFunctionPermsCtx     :: !FunctionPermissionsCtx
  , _sccRemoteSchemaPermsCtx :: !RemoteSchemaPermsCtx
  , _sccSQLGenCtx            :: !SQLGenCtx
  , _sccMaintenanceMode      :: !MaintenanceMode
  , _sccExperimentalFeatures :: !(Set.HashSet ExperimentalFeature)
  } deriving (Show, Eq)
