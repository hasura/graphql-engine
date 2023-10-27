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
    sqlserverLivenessCheckAttempts,
    sqlserverLivenessCheckIntervalSeconds,
    sqlserverConnectInfo,
    sqlserverAdminConnectInfo,
    sqlserverDb,
    bigqueryServiceKeyVar,
    bigqueryProjectIdVar,
    bigqueryDataset,
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

import Data.HashSet qualified as Set
import Data.Word (Word16)
import Database.PG.Query qualified as PG
import Harness.UniqueTestId (UniqueTestId)
import Hasura.Backends.Postgres.Connection.MonadTx (ExtensionsSchema (..))
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (emptyMetadataDefaults)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Cors (CorsConfig (CCAllowAll))
import Hasura.Server.Init
  ( API (CONFIG, DEVELOPER, GRAPHQL, METADATA),
    OptionalInterval (..),
    ServeOptions (..),
  )
import Hasura.Server.Init qualified as Init
import Hasura.Server.Logging (MetadataQueryLoggingMode (MetadataQueryLoggingDisabled))
import Hasura.Server.Types
  ( ApolloFederationStatus (ApolloFederationDisabled),
    EventingMode (EventingEnabled),
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
  "postgresql://"
    <> postgresMetadataUser
    <> ":"
    <> postgresMetadataPassword
    <> "@"
    <> postgresMetadataHost
    <> ":"
    <> show postgresMetadataPort
    <> "/"
    <> postgresMetadataDb

-- * Postgres

postgresPassword :: Text
postgresPassword = "hasura"

postgresUser :: Text
postgresUser = "hasura"

postgresDb :: Text
postgresDb = "hasura"

postgresHost :: Text
postgresHost = "127.0.0.1"

postgresPort :: Word16
postgresPort = 65002

defaultPostgresPort :: Word16
defaultPostgresPort = 5432

-- | return a unique database name from our TestEnvironment's uniqueTestId
uniqueDbName :: UniqueTestId -> Text
uniqueDbName uniqueTestId = "test" <> tshow uniqueTestId

-- * Citus

citusPassword :: Text
citusPassword = "hasura"

citusUser :: Text
citusUser = "hasura"

citusDb :: Text
citusDb = "hasura"

citusHost :: Text
citusHost = "127.0.0.1"

citusPort :: Word16
citusPort = 65004

citusConnectionString :: UniqueTestId -> Text
citusConnectionString uniqueTestId =
  "postgresql://"
    <> citusUser
    <> ":"
    <> citusPassword
    <> "@"
    <> citusHost
    <> ":"
    <> tshow citusPort
    <> "/"
    <> uniqueDbName uniqueTestId

defaultCitusConnectionString :: Text
defaultCitusConnectionString =
  "postgresql://"
    <> citusUser
    <> ":"
    <> citusPassword
    <> "@"
    <> citusHost
    <> ":"
    <> tshow citusPort
    <> "/"
    <> citusDb

-- * Cockroach

cockroachUser :: Text
cockroachUser = "hasura"

cockroachDb :: Text
cockroachDb = "hasura"

cockroachHost :: Text
cockroachHost = "127.0.0.1"

cockroachPort :: Word16
cockroachPort = 65008

cockroachConnectionString :: UniqueTestId -> Text
cockroachConnectionString uniqueTestId =
  "postgresql://"
    <> cockroachUser
    <> "@"
    <> cockroachHost
    <> ":"
    <> tshow cockroachPort
    <> "/"
    <> uniqueDbName uniqueTestId
    <> "?sslmode=disable"

defaultCockroachConnectionString :: Text
defaultCockroachConnectionString =
  "postgresql://"
    <> cockroachUser
    <> "@"
    <> cockroachHost
    <> ":"
    <> tshow cockroachPort
    <> "/"
    <> cockroachDb
    <> "?sslmode=disable"

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
sqlserverConnectInfo :: UniqueTestId -> Text
sqlserverConnectInfo uniqueTestId =
  let dbName = uniqueDbName uniqueTestId
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

-- * Server configuration

serveOptions :: ServeOptions L.Hasura
serveOptions =
  ServeOptions
    { soPort = Init.unsafePort 12345, -- The server runner will typically be generating
    -- a random port, so this isn't particularly
    -- important.
      soHost = "0.0.0.0",
      soConnParams =
        PG.defaultConnParams
          { PG.cpConns = 3, -- only use three simultaneous connection
            PG.cpTimeout = Just 30, -- pool should only wait 30 seconds for a connection to stop tests hanging
            PG.cpStripes = 2 -- two thread pls
          },
      soTxIso = PG.Serializable,
      soAdminSecret = mempty,
      soAuthHook = Nothing,
      soJwtSecret = mempty,
      soUnAuthRole = Nothing,
      soCorsConfig = CCAllowAll,
      soConsoleStatus = Init.ConsoleEnabled,
      soConsoleAssetsDir = Just "frontend/dist/apps/server-assets-console-ce",
      soConsoleSentryDsn = Nothing,
      soEnableTelemetry = Init.TelemetryDisabled,
      soStringifyNum = Options.Don'tStringifyNumbers,
      soDangerousBooleanCollapse = Options.Don'tDangerouslyCollapseBooleans,
      soRemoteNullForwardingPolicy = Options.RemoteForwardAccurately,
      soEnabledAPIs = testSuiteEnabledApis,
      soLiveQueryOpts = ES.mkSubscriptionsOptions Nothing Nothing,
      soStreamingQueryOpts = ES.mkSubscriptionsOptions Nothing Nothing,
      soEnableAllowList = Init.AllowListDisabled,
      soEnabledLogTypes = Set.fromList L.userAllowedLogTypes,
      soLogLevel = fromMaybe (L.LevelOther "test-suite") engineLogLevel,
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
      soDevMode = Init.DevModeEnabled,
      soAdminInternalErrors = Init.AdminInternalErrorsEnabled,
      soGracefulShutdownTimeout = $$(refineTH 0), -- Don't wait to shutdown.
      soWebSocketConnectionInitTimeout = Init._default Init.webSocketConnectionInitTimeoutOption,
      soEventingMode = EventingEnabled,
      soReadOnlyMode = ReadOnlyModeDisabled,
      soEnableMetadataQueryLogging = MetadataQueryLoggingDisabled,
      soDefaultNamingConvention = Init._default Init.defaultNamingConventionOption,
      soExtensionsSchema = ExtensionsSchema "public",
      soMetadataDefaults = emptyMetadataDefaults,
      soApolloFederationStatus = ApolloFederationDisabled,
      soCloseWebsocketsOnMetadataChangeStatus = Init._default Init.closeWebsocketsOnMetadataChangeOption,
      soMaxTotalHeaderLength = Init._default Init.maxTotalHeaderLengthOption,
      soTriggersErrorLogLevelStatus = Init._default Init.triggersErrorLogLevelStatusOption,
      soAsyncActionsFetchBatchSize = Init._default Init.asyncActionsFetchBatchSizeOption,
      soPersistedQueries = Init._default Init.persistedQueriesOption,
      soPersistedQueriesTtl = Init._default Init.persistedQueriesTtlOption
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
