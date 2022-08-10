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
    mysqlLivenessCheckAttempts,
    mysqlLivenessCheckIntervalSeconds,
    mysqlPassword,
    mysqlUser,
    mysqlDb,
    mysqlHost,
    mysqlPort,
    mysqlConnectInfo,
    sqlserverLivenessCheckAttempts,
    sqlserverLivenessCheckIntervalSeconds,
    sqlserverConnectInfo,
    sqlserverDb,
    bigqueryServiceKeyVar,
    bigqueryProjectIdVar,
    bigqueryDataset,
    httpHealthCheckAttempts,
    httpHealthCheckIntervalSeconds,
    citusConnectionString,
    citusDb,
    serveOptions,
    dataConnectorDb,
    maxRetriesRateLimitExceeded,
  )
where

-------------------------------------------------------------------------------

import Data.HashSet qualified as Set
import Data.Word (Word16)
import Database.MySQL.Simple qualified as Mysql
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection.MonadTx (ExtensionsSchema (..))
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.Server.Cors (CorsConfig (CCAllowAll))
import Hasura.Server.Init
  ( API (CONFIG, DEVELOPER, GRAPHQL, METADATA),
    OptionalInterval (..),
    ResponseInternalErrorsConfig (..),
    ServeOptions (..),
  )
import Hasura.Server.Init qualified as Init
import Hasura.Server.Logging (MetadataQueryLoggingMode (MetadataQueryLoggingDisabled))
import Hasura.Server.Types
  ( EventingMode (EventingEnabled),
    ExperimentalFeature (EFStreamingSubscriptions),
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

-- * DataConnector

dataConnectorDb :: String
dataConnectorDb = "data-connector"

-- * Liveness

postgresLivenessCheckAttempts :: Int
postgresLivenessCheckAttempts = 5

postgresLivenessCheckIntervalSeconds :: DiffTime
postgresLivenessCheckIntervalSeconds = 1

sqlserverLivenessCheckAttempts :: Int
sqlserverLivenessCheckAttempts = 5

sqlserverLivenessCheckIntervalSeconds :: DiffTime
sqlserverLivenessCheckIntervalSeconds = 1

-- | SQL Server has strict password requirements, that's why it's not
-- simply @hasura@ like the others.
sqlserverConnectInfo :: Text
sqlserverConnectInfo = "DRIVER={ODBC Driver 17 for SQL Server};SERVER=127.0.0.1,65003;Uid=hasura;Pwd=Hasura1!;Encrypt=no"

sqlserverDb :: String
sqlserverDb = "hasura"

mysqlLivenessCheckAttempts :: Int
mysqlLivenessCheckAttempts = 5

mysqlLivenessCheckIntervalSeconds :: DiffTime
mysqlLivenessCheckIntervalSeconds = 1

-- * MySQL

mysqlPassword :: String
mysqlPassword = "hasura"

mysqlUser :: String
mysqlUser = "hasura"

mysqlDb :: String
mysqlDb = "hasura"

mysqlHost :: String
mysqlHost = "127.0.0.1"

mysqlPort :: Word16
mysqlPort = 65001

mysqlConnectInfo :: Mysql.ConnectInfo
mysqlConnectInfo =
  Mysql.defaultConnectInfo
    { Mysql.connectUser = mysqlUser,
      Mysql.connectPassword = mysqlPassword,
      Mysql.connectDatabase = mysqlDb,
      Mysql.connectHost = mysqlHost,
      Mysql.connectPort = mysqlPort
    }

bigqueryServiceKeyVar :: String
bigqueryServiceKeyVar = "HASURA_BIGQUERY_SERVICE_KEY"

bigqueryProjectIdVar :: String
bigqueryProjectIdVar = "HASURA_BIGQUERY_PROJECT_ID"

bigqueryDataset :: String
bigqueryDataset = "hasura"

-- * HTTP health checks

httpHealthCheckAttempts :: Int
httpHealthCheckAttempts = 5

httpHealthCheckIntervalSeconds :: DiffTime
httpHealthCheckIntervalSeconds = 1

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
      soConsoleAssetsDir = Just "../console/static/dist",
      soEnableTelemetry = False,
      soStringifyNum = Options.StringifyNumbers,
      soDangerousBooleanCollapse = False,
      soEnabledAPIs = testSuiteEnabledApis,
      soLiveQueryOpts = ES.mkSubscriptionsOptions Nothing Nothing,
      soStreamingQueryOpts = ES.mkSubscriptionsOptions Nothing Nothing,
      soEnableAllowlist = False,
      soEnabledLogTypes = Set.fromList L.userAllowedLogTypes,
      soLogLevel = fromMaybe (L.LevelOther "test-suite") engineLogLevel,
      soResponseInternalErrorsConfig = InternalErrorsAllRequests,
      soEventsHttpPoolSize = Init._default Init.graphqlEventsHttpPoolSizeOption,
      soEventsFetchInterval = Init._default Init.graphqlEventsFetchIntervalOption,
      soAsyncActionsFetchInterval = Skip,
      soEnableRemoteSchemaPermissions = Options.DisableRemoteSchemaPermissions,
      soConnectionOptions = WS.defaultConnectionOptions,
      soWebSocketKeepAlive = Init._default Init.webSocketKeepAliveOption,
      soInferFunctionPermissions = Options.InferFunctionPermissions,
      soEnableMaintenanceMode = MaintenanceModeDisabled,
      -- MUST be disabled to be able to modify schema.
      soSchemaPollInterval = Interval 10,
      soExperimentalFeatures = Set.singleton EFStreamingSubscriptions,
      soEventsFetchBatchSize = 1,
      soDevMode = True,
      soGracefulShutdownTimeout = 0, -- Don't wait to shutdown.
      soWebSocketConnectionInitTimeout = Init._default Init.webSocketConnectionInitTimeoutOption,
      soEventingMode = EventingEnabled,
      soReadOnlyMode = ReadOnlyModeDisabled,
      soEnableMetadataQueryLogging = MetadataQueryLoggingDisabled,
      soDefaultNamingConvention = Nothing,
      soExtensionsSchema = ExtensionsSchema "public"
    }

-- | What log level should be used by the engine; this is not exported, and
-- only used in 'serveOptions'.
--
-- This should be adjusted locally for debugging purposes; e.g. change it to
-- @Just L.LevelDebug@ to enable all logs.
--
-- See 'L.LogLevel' for an enumeration of available log levels.
engineLogLevel :: Maybe L.LogLevel
engineLogLevel = Nothing

-- These are important for the test suite.
testSuiteEnabledApis :: HashSet API
testSuiteEnabledApis = Set.fromList [METADATA, GRAPHQL, DEVELOPER, CONFIG]

maxRetriesRateLimitExceeded :: Int
maxRetriesRateLimitExceeded = 4
