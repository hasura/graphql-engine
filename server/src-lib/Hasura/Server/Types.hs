module Hasura.Server.Types where

import           Hasura.Prelude

import           Data.Aeson

import qualified Database.PG.Query             as Q
import qualified Network.HTTP.Types            as HTTP

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.Server.Utils

newtype RequestId
  = RequestId { unRequestId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

getRequestId :: (MonadIO m) => [HTTP.Header] -> m RequestId
getRequestId headers =
  -- generate a request id for every request if the client has not sent it
  case getRequestHeader requestIdHeader headers  of
    Nothing    -> RequestId <$> liftIO generateFingerprint
    Just reqId -> return $ RequestId $ bsToTxt reqId

newtype DbUid
  = DbUid { getDbUid :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype PGVersion
  = PGVersion { unPGVersion :: Int }
  deriving (Show, Eq, ToJSON)

newtype InstanceId
  = InstanceId { getInstanceId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON, Q.FromCol, Q.ToPrepArg)

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
  } deriving (Show, Eq)
