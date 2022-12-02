{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Constant configurations used throughout the test suite.
module Harness.Constants
  ( postgresPassword,
    postgresUser,
    postgresDb,
    postgresHost,
    postgresPort,
    postgresqlMetadataConnectionString,
    postgresLivenessCheckAttempts,
    postgresLivenessCheckIntervalSeconds,
    defaultPostgresPort,
    sqlserverLivenessCheckAttempts,
    sqlserverLivenessCheckIntervalSeconds,
    sqlserverConnectInfo,
    sqlserverAdminConnectInfo,
    sqlserverDb,
    bigqueryServiceKeyVar,
    bigqueryProjectIdVar,
    bigqueryDataset,
    httpHealthCheckAttempts,
    httpHealthCheckIntervalSeconds,
    citusConnectionString,
    citusDb,
    defaultCitusConnectionString,
    cockroachConnectionString,
    defaultCockroachConnectionString,
    cockroachDb,
    serveOptions,
    dataConnectorDb,
    sqliteSchemaName,
    maxRetriesRateLimitExceeded,
    uniqueDbName,
  )
where

-------------------------------------------------------------------------------

import Data.Char qualified
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.Word (Word16)
import Database.PG.Query qualified as PG
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Backends.Postgres.Connection.MonadTx (ExtensionsSchema (..))
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (emptyMetadataDefaults)
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
    ExperimentalFeature (..),
    MaintenanceMode (MaintenanceModeDisabled),
    ReadOnlyMode (ReadOnlyModeDisabled),
  )
import Network.WebSockets qualified as WS
import Refined (refineTH)

-------------------------------------------------------------------------------

-- * Postgres metadata DB

-- To allow us to test different Postgres flavours,
-- we have separate connection details for the metadata DB
-- (which should always be Postgres) and other Postgres (which
-- might actually be Yugabyte, etc)
postgresMetadataPassword :: String
postgresMetadataPassword = "hasura"

postgresMetadataUser :: String
postgresMetadataUser = "hasura"

postgresMetadataDb :: String
postgresMetadataDb = "hasura_metadata"

postgresMetadataHost :: String
postgresMetadataHost = "127.0.0.1"

postgresMetadataPort :: Word16
postgresMetadataPort = 65002

postgresqlMetadataConnectionString :: String
postgresqlMetadataConnectionString =
  "postgres://"
    ++ postgresMetadataUser
    ++ ":"
    ++ postgresMetadataPassword
    ++ "@"
    ++ postgresMetadataHost
    ++ ":"
    ++ show postgresMetadataPort
    ++ "/"
    ++ postgresMetadataDb

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

defaultPostgresPort :: Word16
defaultPostgresPort = 5432

-- | return a unique database name from our TestEnvironment's uniqueTestId
uniqueDbName :: UUID -> String
uniqueDbName uuid = "test" <> showUUID uuid

-- | Sanitise UUID for use in BigQuery dataset name
-- must be alphanumeric (plus underscores)
showUUID :: UUID -> String
showUUID =
  map
    ( \a ->
        if Data.Char.isAlphaNum a
          then a
          else '_'
    )
    . show

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

citusConnectionString :: TestEnvironment -> String
citusConnectionString testEnv =
  "postgres://"
    ++ citusUser
    ++ ":"
    ++ citusPassword
    ++ "@"
    ++ citusHost
    ++ ":"
    ++ show citusPort
    ++ "/"
    ++ uniqueDbName (uniqueTestId testEnv)

defaultCitusConnectionString :: String
defaultCitusConnectionString =
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

-- * Cockroach

cockroachUser :: String
cockroachUser = "root"

cockroachDb :: String
cockroachDb = "hasura"

cockroachHost :: String
cockroachHost = "127.0.0.1"

cockroachPort :: Word16
cockroachPort = 65008

cockroachConnectionString :: TestEnvironment -> String
cockroachConnectionString testEnvironment =
  "postgresql://"
    ++ cockroachUser
    ++ "@"
    ++ cockroachHost
    ++ ":"
    ++ show cockroachPort
    ++ "/"
    ++ uniqueDbName (uniqueTestId testEnvironment)
    ++ "?sslmode=disable"

defaultCockroachConnectionString :: String
defaultCockroachConnectionString =
  "postgresql://"
    ++ cockroachUser
    ++ "@"
    ++ cockroachHost
    ++ ":"
    ++ show cockroachPort
    ++ "/"
    ++ cockroachDb
    ++ "?sslmode=disable"

-- * DataConnector

dataConnectorDb :: String
dataConnectorDb = "hasura"

sqliteSchemaName :: Text
sqliteSchemaName = "main"

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
-- connection info for admin (with CREATE DATABASE permissions)
sqlserverAdminConnectInfo :: Text
sqlserverAdminConnectInfo = "DRIVER={ODBC Driver 18 for SQL Server};SERVER=127.0.0.1,65003;Uid=sa;Pwd=Password!;Encrypt=optional"

-- | SQL Server has strict password requirements, that's why it's not
-- simply @hasura@ like the others.
sqlserverConnectInfo :: TestEnvironment -> Text
sqlserverConnectInfo testEnvironment =
  let dbName = T.pack $ uniqueDbName $ uniqueTestId testEnvironment
   in "DRIVER={ODBC Driver 18 for SQL Server};SERVER=127.0.0.1,65003;Uid=sa;Pwd=Password!;Database="
        <> dbName
        <> ";Encrypt=optional"

sqlserverDb :: String
sqlserverDb = "hasura"

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
    { soPort = Init.unsafePort 12345, -- The server runner will typically be generating
    -- a random port, so this isn't particularly
    -- important.
      soHost = "0.0.0.0",
      soConnParams = PG.defaultConnParams,
      soTxIso = PG.Serializable,
      soAdminSecret = mempty,
      soAuthHook = Nothing,
      soJwtSecret = mempty,
      soUnAuthRole = Nothing,
      soCorsConfig = CCAllowAll,
      soEnableConsole = True,
      soConsoleAssetsDir = Just "../../../console/static/dist",
      soConsoleSentryDsn = Nothing,
      soEnableTelemetry = False,
      soStringifyNum = Options.Don'tStringifyNumbers,
      soDangerousBooleanCollapse = Options.Don'tDangerouslyCollapseBooleans,
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
      soSchemaPollInterval = Interval $$(refineTH 10),
      soExperimentalFeatures = Set.fromList [EFStreamingSubscriptions, EFBigQueryStringNumericInput],
      soEventsFetchBatchSize = $$(refineTH 1),
      soDevMode = True,
      soGracefulShutdownTimeout = $$(refineTH 0), -- Don't wait to shutdown.
      soWebSocketConnectionInitTimeout = Init._default Init.webSocketConnectionInitTimeoutOption,
      soEventingMode = EventingEnabled,
      soReadOnlyMode = ReadOnlyModeDisabled,
      soEnableMetadataQueryLogging = MetadataQueryLoggingDisabled,
      soDefaultNamingConvention = Nothing,
      soExtensionsSchema = ExtensionsSchema "public",
      soMetadataDefaults = emptyMetadataDefaults
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
