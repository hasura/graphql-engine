{-# LANGUAGE DisambiguateRecordFields #-}

-- | Constant configurations used throughout the test suite.
module Harness.Constants
  ( postgresPassword,
    postgresUser,
    postgresDb,
    postgresHost,
    postgresPort,
    postgresqlConnectionString,
    postgresLivenessCheckAttempts,
    postgresLivenessCheckIntervalSeconds,
    postgresLivenessCheckIntervalMicroseconds,
    mysqlLivenessCheckAttempts,
    mysqlLivenessCheckIntervalSeconds,
    mysqlLivenessCheckIntervalMicroseconds,
    mysqlPassword,
    mysqlUser,
    mysqlDatabase,
    mysqlHost,
    mysqlPort,
    mysqlConnectInfo,
    sqlserverLivenessCheckAttempts,
    sqlserverLivenessCheckIntervalSeconds,
    sqlserverLivenessCheckIntervalMicroseconds,
    sqlserverConnectInfo,
    sqlserverDb,
    bigqueryServiceAccountFileVar,
    bigqueryServiceAccountVar,
    bigqueryProjectIdVar,
    bigqueryDataset,
    httpHealthCheckAttempts,
    httpHealthCheckIntervalSeconds,
    httpHealthCheckIntervalMicroseconds,
    citusConnectionString,
    citusDb,
    serveOptions,
  )
where

-------------------------------------------------------------------------------

import Data.HashSet qualified as Set
import Data.Word (Word16)
import Database.MySQL.Simple qualified as Mysql
import Database.PG.Query qualified as Q
import Hasura.GraphQL.Execute.LiveQuery.Options qualified as LQ
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types
  ( FunctionPermissionsCtx (FunctionPermissionsInferred),
    RemoteSchemaPermsCtx (RemoteSchemaPermsDisabled),
  )
import Hasura.RQL.Types.Common (StringifyNumbers (..))
import Hasura.Server.Cors (CorsConfig (CCAllowAll))
import Hasura.Server.Init
  ( API (CONFIG, DEVELOPER, GRAPHQL, METADATA),
    OptionalInterval (..),
    ResponseInternalErrorsConfig (..),
    ServeOptions (..),
  )
import Hasura.Server.Init qualified as Init
import Hasura.Server.Types
  ( EventingMode (EventingEnabled),
    MaintenanceMode (MaintenanceModeDisabled),
    ReadOnlyMode (ReadOnlyModeDisabled),
  )
import Network.WebSockets qualified as WS

-------------------------------------------------------------------------------

-- * Postgres

postgresPassword :: String
postgresPassword = "hasura"

postgresUser :: String
postgresUser = "hasura"

postgresDb :: String
postgresDb = "hasura"

postgresHost :: String
postgresHost = "127.0.0.1"

postgresPort :: Word16
postgresPort = 65002

postgresqlConnectionString :: String
postgresqlConnectionString =
  "postgres://"
    ++ postgresUser
    ++ ":"
    ++ postgresPassword
    ++ "@"
    ++ postgresHost
    ++ ":"
    ++ show postgresPort
    ++ "/"
    ++ postgresDb

-- * Citus

citusPassword :: String
citusPassword = "hasura"

citusUser :: String
citusUser = "hasura"

citusDb :: String
citusDb = "hasura"

citusHost :: String
citusHost = "127.0.0.1"

citusPort :: Word16
citusPort = 65004

citusConnectionString :: String
citusConnectionString =
  "postgres://"
    ++ citusUser
    ++ ":"
    ++ citusPassword
    ++ "@"
    ++ citusHost
    ++ ":"
    ++ show citusPort
    ++ "/"
    ++ citusDb

-- * Liveness

postgresLivenessCheckAttempts :: Int
postgresLivenessCheckAttempts = 5

postgresLivenessCheckIntervalSeconds :: Int
postgresLivenessCheckIntervalSeconds = 1

sqlserverLivenessCheckAttempts :: Int
sqlserverLivenessCheckAttempts = 5

sqlserverLivenessCheckIntervalSeconds :: Int
sqlserverLivenessCheckIntervalSeconds = 1

-- | SQL Server has strict password requirements, that's why it's not
-- simply @hasura@ like the others.
sqlserverConnectInfo :: Text
sqlserverConnectInfo = "DRIVER={ODBC Driver 17 for SQL Server};SERVER=127.0.0.1,65003;Uid=hasura;Pwd=Hasura1!;Encrypt=no"

sqlserverDb :: String
sqlserverDb = "hasura"

sqlserverLivenessCheckIntervalMicroseconds :: Int
sqlserverLivenessCheckIntervalMicroseconds = 1000 * 1000 * sqlserverLivenessCheckIntervalSeconds

postgresLivenessCheckIntervalMicroseconds :: Int
postgresLivenessCheckIntervalMicroseconds = 1000 * 1000 * postgresLivenessCheckIntervalSeconds

mysqlLivenessCheckAttempts :: Int
mysqlLivenessCheckAttempts = 5

mysqlLivenessCheckIntervalSeconds :: Int
mysqlLivenessCheckIntervalSeconds = 1

mysqlLivenessCheckIntervalMicroseconds :: Int
mysqlLivenessCheckIntervalMicroseconds = 1000 * 1000 * mysqlLivenessCheckIntervalSeconds

-- * MySQL

mysqlPassword :: String
mysqlPassword = "hasura"

mysqlUser :: String
mysqlUser = "hasura"

mysqlDatabase :: String
mysqlDatabase = "hasura"

mysqlHost :: String
mysqlHost = "127.0.0.1"

mysqlPort :: Word16
mysqlPort = 65001

mysqlConnectInfo :: Mysql.ConnectInfo
mysqlConnectInfo =
  Mysql.defaultConnectInfo
    { Mysql.connectUser = mysqlUser,
      Mysql.connectPassword = mysqlPassword,
      Mysql.connectDatabase = mysqlDatabase,
      Mysql.connectHost = mysqlHost,
      Mysql.connectPort = mysqlPort
    }

bigqueryServiceAccountFileVar :: String
bigqueryServiceAccountFileVar = "HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE"

bigqueryServiceAccountVar :: String
bigqueryServiceAccountVar = "HASURA_BIGQUERY_SERVICE_ACCOUNT"

bigqueryProjectIdVar :: String
bigqueryProjectIdVar = "HASURA_BIGQUERY_PROJECT_ID"

bigqueryDataset :: String
bigqueryDataset = "hasura"

-- * HTTP health checks

httpHealthCheckAttempts :: Int
httpHealthCheckAttempts = 5

httpHealthCheckIntervalSeconds :: Int
httpHealthCheckIntervalSeconds = 1

httpHealthCheckIntervalMicroseconds :: Int
httpHealthCheckIntervalMicroseconds = 1000 * 1000 * httpHealthCheckIntervalSeconds

-- * Server configuration

serveOptions :: ServeOptions L.Hasura
serveOptions =
  ServeOptions
    { soPort = 12345, -- The server runner will typically be generating
    -- a random port, so this isn't particularly
    -- important.
      soHost = "0.0.0.0",
      soConnParams = Q.defaultConnParams,
      soTxIso = Q.Serializable,
      soAdminSecret = mempty,
      soAuthHook = Nothing,
      soJwtSecret = mempty,
      soUnAuthRole = Nothing,
      soCorsConfig = CCAllowAll,
      soEnableConsole = True,
      soConsoleAssetsDir = Nothing,
      soEnableTelemetry = False,
      soStringifyNum = StringifyNumbers,
      soDangerousBooleanCollapse = False,
      soEnabledAPIs = testSuiteEnabledApis,
      soLiveQueryOpts = LQ.mkLiveQueriesOptions Nothing Nothing,
      soEnableAllowlist = False,
      soEnabledLogTypes = Set.fromList L.userAllowedLogTypes,
      soLogLevel = fromMaybe (L.LevelOther "test-suite") engineLogLevel,
      soResponseInternalErrorsConfig = InternalErrorsAllRequests,
      soEventsHttpPoolSize = Nothing,
      soEventsFetchInterval = Nothing,
      soAsyncActionsFetchInterval = Skip,
      soLogHeadersFromEnv = False,
      soEnableRemoteSchemaPermissions = RemoteSchemaPermsDisabled,
      soConnectionOptions = WS.defaultConnectionOptions,
      soWebsocketKeepAlive = Init.defaultKeepAliveDelay,
      soInferFunctionPermissions = FunctionPermissionsInferred,
      soEnableMaintenanceMode = MaintenanceModeDisabled,
      -- MUST be disabled to be able to modify schema.
      soSchemaPollInterval = Interval 10,
      soExperimentalFeatures = mempty,
      soEventsFetchBatchSize = 1,
      soDevMode = True,
      soGracefulShutdownTimeout = 0, -- Don't wait to shutdown.
      soWebsocketConnectionInitTimeout = Init.defaultWSConnectionInitTimeout,
      soEventingMode = EventingEnabled,
      soReadOnlyMode = ReadOnlyModeDisabled
    }

-- | What log level should be used by the engine; this is not exported, and
-- only used in 'serveOptions'.
--
-- This should be adjusted locally for debugging purposes; e.g. change it to
-- @Just L.Debug@ to enable all logs.
--
-- See 'L.LogLevel' for an enumeration of available log levels.
engineLogLevel :: Maybe L.LogLevel
engineLogLevel = Nothing

-- These are important for the test suite.
testSuiteEnabledApis :: HashSet API
testSuiteEnabledApis = Set.fromList [METADATA, GRAPHQL, DEVELOPER, CONFIG]
