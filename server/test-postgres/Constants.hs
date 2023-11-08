{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Constant configurations used throughout the test suite.
module Constants
  ( serveOptions,
  )
where

-------------------------------------------------------------------------------

import Data.HashSet qualified as Set
import Database.PG.Query qualified as PG
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
      soConsoleAssetsDir = Just "../../frontend/dist/apps/server-assets-console-ce",
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
