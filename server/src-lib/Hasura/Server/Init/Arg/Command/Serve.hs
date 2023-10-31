{-# LANGUAGE TemplateHaskell #-}

-- | The Arg Opt.Parser for the 'serve' subcommand.
module Hasura.Server.Init.Arg.Command.Serve
  ( -- * Opt.Parser
    serveCommandParser,

    -- * Options
    servePortOption,
    serveHostOption,
    pgStripesOption,
    pgConnsOption,
    pgTimeoutOption,
    pgConnLifetimeOption,
    pgUsePreparedStatementsOption,
    pgPoolTimeoutOption,
    txIsolationOption,
    adminSecretOption,
    accessKeyOption,
    authHookOption,
    authHookModeOption,
    authHookSendRequestBodyOption,
    jwtSecretOption,
    unAuthRoleOption,
    corsDomainOption,
    disableCorsOption,
    enableConsoleOption,
    consoleAssetsDirOption,
    consoleSentryDsnOption,
    enableTelemetryOption,
    wsReadCookieOption,
    stringifyNumOption,
    dangerousBooleanCollapseOption,
    remoteNullForwardingPolicyOption,
    enabledAPIsOption,
    mxRefetchDelayOption,
    mxBatchSizeOption,
    streamingMxRefetchDelayOption,
    streamingMxBatchSizeOption,
    enableAllowlistOption,
    enabledLogsOption,
    logLevelOption,
    graphqlDevModeOption,
    graphqlAdminInternalErrorsOption,
    graphqlEventsHttpPoolSizeOption,
    graphqlEventsFetchIntervalOption,
    asyncActionsFetchIntervalOption,
    enableRemoteSchemaPermsOption,
    webSocketCompressionOption,
    webSocketKeepAliveOption,
    inferFunctionPermsOption,
    enableMaintenanceModeOption,
    schemaPollIntervalOption,
    experimentalFeaturesOption,
    eventsFetchBatchSizeOption,
    gracefulShutdownOption,
    webSocketConnectionInitTimeoutOption,
    enableMetadataQueryLoggingOption,
    defaultNamingConventionOption,
    metadataDBExtensionsSchemaOption,
    parseMetadataDefaults,
    metadataDefaultsOption,
    apolloFederationStatusOption,
    triggersErrorLogLevelStatusOption,
    closeWebsocketsOnMetadataChangeOption,
    maxTotalHeaderLengthOption,
    asyncActionsFetchBatchSizeOption,
    persistedQueriesOption,
    persistedQueriesTtlOption,

    -- * Pretty Printer
    serveCmdFooter,
  )
where

--------------------------------------------------------------------------------

import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Data.Time qualified as Time
import Database.PG.Query qualified as Query
import Hasura.Backends.Postgres.Connection.MonadTx qualified as MonadTx
import Hasura.Cache.Bounded qualified as Bounded
import Hasura.GraphQL.Execute.Subscription.Options qualified as Subscription.Options
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (MetadataDefaults, emptyMetadataDefaults)
import Hasura.RQL.Types.NamingCase qualified as NC
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Roles qualified as Roles
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init.Arg.PrettyPrinter qualified as PP
import Hasura.Server.Init.Config qualified as Config
import Hasura.Server.Init.Env qualified as Env
import Hasura.Server.Logging qualified as Server.Logging
import Hasura.Server.Types qualified as Types
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WebSockets
import Options.Applicative qualified as Opt
import Refined (NonNegative, Positive, Refined, refineTH)
import Witch qualified

--------------------------------------------------------------------------------
-- Serve Command

serveCommandParser :: (Logging.EnabledLogTypes impl) => Opt.Parser (Config.ServeOptionsRaw impl)
serveCommandParser =
  Config.ServeOptionsRaw
    <$> parseServerPort
    <*> parseServerHost
    <*> parseConnParams
    <*> parseTxIsolation
    <*> (parseAdminSecret <|> parseAccessKey)
    <*> parseAuthHook
    <*> parseJwtSecret
    <*> parseUnAuthRole
    <*> parseCorsConfig
    <*> parseEnableConsole
    <*> parseConsoleAssetsDir
    <*> parseConsoleSentryDsn
    <*> parseEnableTelemetry
    <*> parseWsReadCookie
    <*> parseStringifyNum
    <*> parseDangerousBooleanCollapse
    <*> parseRemoteNullForwardingPolicy
    <*> parseEnabledAPIs
    <*> parseMxRefetchDelay
    <*> parseMxBatchSize
    <*> parseStreamingMxRefetchDelay
    <*> parseStreamingMxBatchSize
    <*> parseEnableAllowlist
    <*> parseEnabledLogs
    <*> parseLogLevel
    <* parsePlanCacheSize -- parsed (for backwards compatibility reasons) but ignored
    <*> parseGraphqlDevMode
    <*> parseGraphqlAdminInternalErrors
    <*> parseGraphqlEventsHttpPoolSize
    <*> parseGraphqlEventsFetchInterval
    <*> parseGraphqlAsyncActionsFetchInterval
    <*> parseEnableRemoteSchemaPerms
    <*> parseWebSocketCompression
    <*> parseWebSocketKeepAlive
    <*> parseInferFunctionPerms
    <*> parseEnableMaintenanceMode
    <*> parseSchemaPollInterval
    <*> parseExperimentalFeatures
    <*> parseEventsFetchBatchSize
    <*> parseGracefulShutdownTimeout
    <*> parseWebSocketConnectionInitTimeout
    <*> parseEnableMetadataQueryLogging
    <*> parseDefaultNamingConvention
    <*> parseExtensionsSchema
    <*> parseMetadataDefaults
    <*> parseApolloFederationStatus
    <*> parseEnableCloseWebsocketsOnMetadataChange
    <*> parseMaxTotalHeaderLength
    <*> parseTriggersErrorLoglevelStatus
    <*> parseAsyncActionsFetchBatchSize
    <*> parsePersistedQueries
    <*> parsePersistedQueriesTtl

--------------------------------------------------------------------------------
-- Serve Options

parseServerPort :: Opt.Parser (Maybe Config.Port)
parseServerPort =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "server-port"
          <> Opt.metavar "<PORT>"
          <> Opt.help (Config._helpMessage servePortOption)
      )

servePortOption :: Config.Option Config.Port
servePortOption =
  Config.Option
    { _default = Config.unsafePort 8080,
      _envVar = "HASURA_GRAPHQL_SERVER_PORT",
      _helpMessage = "Port on which graphql-engine should be served (default: 8080)"
    }

parseServerHost :: Opt.Parser (Maybe Warp.HostPreference)
parseServerHost =
  Opt.optional
    $ Opt.strOption
      ( Opt.long "server-host"
          <> Opt.metavar "<HOST>"
          <> Opt.help (Config._helpMessage serveHostOption)
      )

serveHostOption :: Config.Option Warp.HostPreference
serveHostOption =
  Config.Option
    { _default = "*",
      _envVar = "HASURA_GRAPHQL_SERVER_HOST",
      _helpMessage = "Host on which graphql-engine will listen (default: *)"
    }

parseConnParams :: Opt.Parser Config.ConnParamsRaw
parseConnParams =
  Config.ConnParamsRaw <$> pgStripes <*> pgConns <*> pgIdleTimeout <*> pgConnLifetime <*> pgUsePreparedStatements <*> pgPoolTimeout
  where
    pgStripes :: Opt.Parser (Maybe (Refined NonNegative Int))
    pgStripes =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "stripes"
              <> Opt.short 's'
              <> Opt.metavar "<NO OF STRIPES>"
              <> Opt.help (Config._helpMessage pgStripesOption)
          )

    pgConns :: Opt.Parser (Maybe (Refined NonNegative Int))
    pgConns =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "connections"
              <> Opt.short 'c'
              <> Opt.metavar "<NO OF CONNS>"
              <> Opt.help (Config._helpMessage pgConnsOption)
          )

    pgIdleTimeout :: Opt.Parser (Maybe (Refined NonNegative Int))
    pgIdleTimeout =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "timeout"
              <> Opt.metavar "<SECONDS>"
              <> Opt.help (Config._helpMessage pgTimeoutOption)
          )

    pgConnLifetime :: Opt.Parser (Maybe (Refined NonNegative Time.NominalDiffTime))
    pgConnLifetime =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "conn-lifetime"
              <> Opt.metavar "<SECONDS>"
              <> Opt.help (Config._helpMessage pgConnLifetimeOption)
          )

    pgUsePreparedStatements :: Opt.Parser (Maybe Bool)
    pgUsePreparedStatements =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "use-prepared-statements"
              <> Opt.metavar "<true|false>"
              <> Opt.help (Config._helpMessage pgUsePreparedStatementsOption)
          )

    pgPoolTimeout :: Opt.Parser (Maybe (Refined NonNegative Time.NominalDiffTime))
    pgPoolTimeout =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "pool-timeout"
              <> Opt.metavar "<SECONDS>"
              <> Opt.help (Config._helpMessage pgPoolTimeoutOption)
          )

pgStripesOption :: Config.Option (Refined NonNegative Int)
pgStripesOption =
  Config.Option
    { _default = $$(refineTH @NonNegative @Int 1),
      _envVar = "HASURA_GRAPHQL_PG_STRIPES",
      _helpMessage =
        "Number of stripes (distinct sub-pools) to maintain with Postgres (default: 1). "
          <> "New connections will be taken from a particular stripe pseudo-randomly."
    }

pgConnsOption :: Config.Option (Refined NonNegative Int)
pgConnsOption =
  Config.Option
    { _default = $$(refineTH @NonNegative @Int 50),
      _envVar = "HASURA_GRAPHQL_PG_CONNECTIONS",
      _helpMessage =
        "Maximum number of Postgres connections that can be opened per stripe (default: 50). "
          <> "When the maximum is reached we will block until a new connection becomes available, "
          <> "even if there is capacity in other stripes."
    }

pgTimeoutOption :: Config.Option (Refined NonNegative Int)
pgTimeoutOption =
  Config.Option
    { _default = $$(refineTH @NonNegative @Int 180),
      _envVar = "HASURA_GRAPHQL_PG_TIMEOUT",
      _helpMessage = "Each connection's idle time before it is closed (default: 180 sec)"
    }

pgConnLifetimeOption :: Config.Option (Refined NonNegative Time.NominalDiffTime)
pgConnLifetimeOption =
  Config.Option
    { _default = $$(refineTH @NonNegative @Time.NominalDiffTime 600),
      _envVar = "HASURA_GRAPHQL_PG_CONN_LIFETIME",
      _helpMessage =
        "Time from connection creation after which the connection should be destroyed and a new one "
          <> "created. A value of 0 indicates we should never destroy an active connection. If 0 is "
          <> "passed, memory from large query results may not be reclaimed. (default: 600 sec)"
    }

pgUsePreparedStatementsOption :: Config.Option Bool
pgUsePreparedStatementsOption =
  Config.Option
    { _default = True,
      _envVar = "HASURA_GRAPHQL_USE_PREPARED_STATEMENTS",
      _helpMessage = "Use prepared statements for queries (default: true)"
    }

pgPoolTimeoutOption :: Config.Option ()
pgPoolTimeoutOption =
  Config.Option
    { _default = (),
      _envVar = "HASURA_GRAPHQL_PG_POOL_TIMEOUT",
      _helpMessage = "How long to wait when acquiring a Postgres connection, in seconds (default: forever)."
    }

parseTxIsolation :: Opt.Parser (Maybe Query.TxIsolation)
parseTxIsolation =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "tx-iso"
          <> Opt.short 'i'
          <> Opt.metavar "<TXISO>"
          <> Opt.help (Config._helpMessage txIsolationOption)
      )

txIsolationOption :: Config.Option Query.TxIsolation
txIsolationOption =
  Config.Option
    { Config._default = Query.ReadCommitted,
      Config._envVar = "HASURA_GRAPHQL_TX_ISOLATION",
      Config._helpMessage = "transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)"
    }

parseAdminSecret :: Opt.Parser (Maybe Auth.AdminSecretHash)
parseAdminSecret =
  Opt.optional
    $ Auth.hashAdminSecret
    <$> Opt.strOption
      ( Opt.long "admin-secret"
          <> Opt.metavar "ADMIN SECRET KEY"
          <> Opt.help (Config._helpMessage adminSecretOption)
      )

adminSecretOption :: Config.Option ()
adminSecretOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_ADMIN_SECRET",
      Config._helpMessage = "Admin Secret key, required to access this instance"
    }

parseAccessKey :: Opt.Parser (Maybe Auth.AdminSecretHash)
parseAccessKey =
  Opt.optional
    $ Auth.hashAdminSecret
    <$> Opt.strOption
      ( Opt.long "access-key"
          <> Opt.metavar "ADMIN SECRET KEY (DEPRECATED: USE --admin-secret)"
          <> Opt.help (Config._helpMessage accessKeyOption)
      )

accessKeyOption :: Config.Option ()
accessKeyOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_ACCESS_KEY",
      Config._helpMessage = "Admin secret key, required to access this instance (deprecated: use HASURA_GRAPHQL_ADMIN_SECRET instead)"
    }

parseAuthHook :: Opt.Parser Config.AuthHookRaw
parseAuthHook =
  Config.AuthHookRaw <$> url <*> urlType <*> sendRequestBody
  where
    url =
      Opt.optional
        $ Opt.strOption
          ( Opt.long "auth-hook"
              <> Opt.metavar "<WEB HOOK URL>"
              <> Opt.help (Config._helpMessage authHookOption)
          )
    urlType =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "auth-hook-mode"
              <> Opt.metavar "<GET|POST>"
              <> Opt.help (Config._helpMessage authHookModeOption)
          )
    sendRequestBody :: Opt.Parser (Maybe Bool) =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "auth-hook-send-request-body"
              <> Opt.metavar "<true|false>"
              <> Opt.help (Config._helpMessage authHookSendRequestBodyOption)
          )

authHookOption :: Config.Option ()
authHookOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_AUTH_HOOK",
      Config._helpMessage = "URL of the authorization webhook required to authorize requests"
    }

authHookModeOption :: Config.Option Auth.AuthHookType
authHookModeOption =
  Config.Option
    { Config._default = Auth.AHTGet,
      Config._envVar = "HASURA_GRAPHQL_AUTH_HOOK_MODE",
      Config._helpMessage = "HTTP method to use for authorization webhook (default: GET)"
    }

authHookSendRequestBodyOption :: Config.Option Bool
authHookSendRequestBodyOption =
  Config.Option
    { Config._default = True,
      Config._envVar = "HASURA_GRAPHQL_AUTH_HOOK_SEND_REQUEST_BODY",
      Config._helpMessage = "Send request body in POST method (default: true)"
    }

parseJwtSecret :: Opt.Parser (Maybe Auth.JWTConfig)
parseJwtSecret =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "jwt-secret"
          <> Opt.metavar "<JSON CONFIG>"
          <> Opt.help (Config._helpMessage jwtSecretOption)
      )

jwtSecretOption :: Config.Option ()
jwtSecretOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_JWT_SECRET",
      Config._helpMessage =
        "The JSON containing type and the JWK used for verifying. e.g: "
          <> "`{\"type\": \"HS256\", \"key\": \"<your-hmac-shared-secret>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`,"
          <> "`{\"type\": \"RS256\", \"key\": \"<your-PEM-RSA-public-key>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`"
    }

parseUnAuthRole :: Opt.Parser (Maybe RoleName)
parseUnAuthRole =
  fmap mkRoleName
    $ Opt.optional
    $ Opt.strOption
      ( Opt.long "unauthorized-role"
          <> Opt.metavar "<ROLE>"
          <> Opt.help (Config._helpMessage unAuthRoleOption)
      )
  where
    mkRoleName mText = mText >>= Roles.mkRoleName

unAuthRoleOption :: Config.Option ()
unAuthRoleOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_UNAUTHORIZED_ROLE",
      Config._helpMessage =
        "Unauthorized role, used when admin-secret is not sent in admin-secret only mode "
          ++ "or \"Authorization\" header is absent in JWT mode"
    }

parseCorsConfig :: Opt.Parser (Maybe Cors.CorsConfig)
parseCorsConfig = mapCC <$> disableCors <*> corsDomain
  where
    corsDomain =
      Opt.optional
        $ Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "cors-domain"
              <> Opt.metavar "<DOMAINS>"
              <> Opt.help (Config._helpMessage corsDomainOption)
          )

    disableCors =
      Opt.switch
        ( Opt.long "disable-cors"
            <> Opt.help (Config._helpMessage disableCorsOption)
        )

    mapCC isDisabled domains =
      bool domains (Just $ Cors.CCDisabled False) isDisabled

corsDomainOption :: Config.Option Cors.CorsConfig
corsDomainOption =
  Config.Option
    { Config._default = Cors.CCAllowAll,
      Config._envVar = "HASURA_GRAPHQL_CORS_DOMAIN",
      Config._helpMessage =
        "CSV of list of domains, excluding scheme (http/https) and including  port, "
          ++ "to allow CORS for. Wildcard domains are allowed. See docs for details."
    }

disableCorsOption :: Config.Option Bool
disableCorsOption =
  Config.Option
    { Config._default = False,
      Config._envVar = "HASURA_GRAPHQL_DISABLE_CORS",
      Config._helpMessage = "Disable CORS. Do not send any CORS headers on any request"
    }

parseEnableConsole :: Opt.Parser Config.ConsoleStatus
parseEnableConsole =
  (bool Config.ConsoleDisabled Config.ConsoleEnabled)
    <$> Opt.switch
      ( Opt.long "enable-console"
          <> Opt.help (Config._helpMessage enableConsoleOption)
      )

enableConsoleOption :: Config.Option Config.ConsoleStatus
enableConsoleOption =
  Config.Option
    { Config._default = Config.ConsoleDisabled,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_CONSOLE",
      Config._helpMessage = "Enable API Console (default: false)"
    }

parseConsoleAssetsDir :: Opt.Parser (Maybe Text)
parseConsoleAssetsDir =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "console-assets-dir"
          <> Opt.help (Config._helpMessage consoleAssetsDirOption)
      )

consoleAssetsDirOption :: Config.Option ()
consoleAssetsDirOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_CONSOLE_ASSETS_DIR",
      Config._helpMessage =
        "A directory from which static assets required for console is served at"
          ++ "'/console/assets' path. Can be set to '/srv/console-assets' on the"
          ++ " default docker image to disable loading assets from CDN."
    }

parseConsoleSentryDsn :: Opt.Parser (Maybe Text)
parseConsoleSentryDsn =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "console-sentry-dsn"
          <> Opt.help (Config._helpMessage consoleSentryDsnOption)
      )

consoleSentryDsnOption :: Config.Option ()
consoleSentryDsnOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_CONSOLE_SENTRY_DSN",
      Config._helpMessage =
        "A Sentry DSN for reporting console errors"
    }

-- NOTE: Should this be an 'Opt.flag'?
parseEnableTelemetry :: Opt.Parser (Maybe Config.TelemetryStatus)
parseEnableTelemetry =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "enable-telemetry"
          <> Opt.help (Config._helpMessage enableTelemetryOption)
      )

enableTelemetryOption :: Config.Option Config.TelemetryStatus
enableTelemetryOption =
  Config.Option
    { _default = Config.TelemetryEnabled,
      _envVar = "HASURA_GRAPHQL_ENABLE_TELEMETRY",
      _helpMessage = "Enable anonymous telemetry on the server and console. For more information, see: https://hasura.io/docs/latest/guides/telemetry (default: true)"
    }

parseWsReadCookie :: Opt.Parser Config.WsReadCookieStatus
parseWsReadCookie =
  bool Config.WsReadCookieDisabled Config.WsReadCookieEnabled
    <$> Opt.switch
      ( Opt.long "ws-read-cookie"
          <> Opt.help (Config._helpMessage wsReadCookieOption)
      )

wsReadCookieOption :: Config.Option Config.WsReadCookieStatus
wsReadCookieOption =
  Config.Option
    { Config._default = Config.WsReadCookieDisabled,
      Config._envVar = "HASURA_GRAPHQL_WS_READ_COOKIE",
      Config._helpMessage =
        "Read cookie on WebSocket initial handshake, even when CORS is disabled."
          ++ " This can be a potential security flaw! Please make sure you know "
          ++ "what you're doing."
          ++ " This configuration is only applicable when CORS is disabled."
    }

parseStringifyNum :: Opt.Parser Options.StringifyNumbers
parseStringifyNum =
  fmap (bool Options.Don'tStringifyNumbers Options.StringifyNumbers)
    $ Opt.switch
      ( Opt.long "stringify-numeric-types"
          <> Opt.help (Config._helpMessage stringifyNumOption)
      )

stringifyNumOption :: Config.Option Options.StringifyNumbers
stringifyNumOption =
  Config.Option
    { Config._default = Options.Don'tStringifyNumbers,
      Config._envVar = "HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES",
      Config._helpMessage = "Stringify numeric types (default: false)"
    }

parseDangerousBooleanCollapse :: Opt.Parser (Maybe Options.DangerouslyCollapseBooleans)
parseDangerousBooleanCollapse =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "v1-boolean-null-collapse"
          <> Opt.help (Config._helpMessage dangerousBooleanCollapseOption)
      )

dangerousBooleanCollapseOption :: Config.Option Options.DangerouslyCollapseBooleans
dangerousBooleanCollapseOption =
  Config.Option
    { Config._default = Options.Don'tDangerouslyCollapseBooleans,
      Config._envVar = "HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE",
      Config._helpMessage =
        "Emulate V1's behaviour re. boolean expression, where an explicit 'null'"
          <> " value will be interpreted to mean that the field should be ignored"
          <> " [DEPRECATED, WILL BE REMOVED SOON] (default: false)"
    }

parseRemoteNullForwardingPolicy :: Opt.Parser (Maybe Options.RemoteNullForwardingPolicy)
parseRemoteNullForwardingPolicy =
  fmap (bool Nothing (Just Options.RemoteOnlyForwardNonNull))
    $ Opt.switch
      ( Opt.long "remote-schema-skip-nulls"
          <> Opt.help (Config._helpMessage remoteNullForwardingPolicyOption)
      )

remoteNullForwardingPolicyOption :: Config.Option Options.RemoteNullForwardingPolicy
remoteNullForwardingPolicyOption =
  Config.Option
    { Config._default = Options.RemoteForwardAccurately,
      Config._envVar = "HASURA_GRAPHQL_REMOTE_SCHEMA_SKIP_NULLS",
      Config._helpMessage =
        "Skip null values from arguments while resolving fields from remote schemas. (default: false, i.e."
          <> " forward null values in argument as well)"
    }

parseEnabledAPIs :: Opt.Parser (Maybe (HashSet Config.API))
parseEnabledAPIs =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "enabled-apis"
          <> Opt.help (Config._helpMessage enabledAPIsOption)
      )

enabledAPIsOption :: Config.Option (HashSet Config.API)
enabledAPIsOption =
  Config.Option
    { Config._default = HashSet.fromList [Config.METADATA, Config.GRAPHQL, Config.PGDUMP, Config.CONFIG],
      Config._envVar = "HASURA_GRAPHQL_ENABLED_APIS",
      Config._helpMessage = "Comma separated list of enabled APIs. (default: metadata,graphql,pgdump,config)"
    }

parseMxRefetchDelay :: Opt.Parser (Maybe Subscription.Options.RefetchInterval)
parseMxRefetchDelay =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "live-queries-multiplexed-refetch-interval"
          <> Opt.metavar "<INTERVAL(ms)>"
          <> Opt.help (Config._envVar mxRefetchDelayOption)
      )

mxRefetchDelayOption :: Config.Option Subscription.Options.RefetchInterval
mxRefetchDelayOption =
  Config.Option
    { Config._default = Subscription.Options.RefetchInterval $$(refineTH 1),
      Config._envVar = "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL",
      Config._helpMessage =
        "results will only be sent once in this interval (in milliseconds) for "
          <> "live queries which can be multiplexed. Default: 1000 (1sec)"
    }

parseMxBatchSize :: Opt.Parser (Maybe Subscription.Options.BatchSize)
parseMxBatchSize =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "live-queries-multiplexed-batch-size"
          <> Opt.metavar "BATCH_SIZE"
          <> Opt.help (Config._helpMessage mxBatchSizeOption)
      )

mxBatchSizeOption :: Config.Option Subscription.Options.BatchSize
mxBatchSizeOption =
  Config.Option
    { _default = Subscription.Options.BatchSize $$(refineTH 100),
      _envVar = "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE",
      _helpMessage =
        "multiplexed live queries are split into batches of the specified "
          <> "size. Default 100. "
    }

parseStreamingMxRefetchDelay :: Opt.Parser (Maybe Subscription.Options.RefetchInterval)
parseStreamingMxRefetchDelay =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "streaming-queries-multiplexed-refetch-interval"
          <> Opt.metavar "<INTERVAL(ms)>"
          <> Opt.help (Config._helpMessage streamingMxRefetchDelayOption)
      )

streamingMxRefetchDelayOption :: Config.Option Subscription.Options.RefetchInterval
streamingMxRefetchDelayOption =
  Config.Option
    { Config._default = Subscription.Options.RefetchInterval $$(refineTH 1),
      Config._envVar = "HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_REFETCH_INTERVAL",
      Config._helpMessage =
        "results will only be sent once in this interval (in milliseconds) for "
          <> "streaming queries which can be multiplexed. Default: 1000 (1sec)"
    }

parseStreamingMxBatchSize :: Opt.Parser (Maybe Subscription.Options.BatchSize)
parseStreamingMxBatchSize =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "streaming-queries-multiplexed-batch-size"
          <> Opt.metavar "BATCH_SIZE"
          <> Opt.help (Config._helpMessage streamingMxBatchSizeOption)
      )

streamingMxBatchSizeOption :: Config.Option Subscription.Options.BatchSize
streamingMxBatchSizeOption =
  Config.Option
    { Config._default = Subscription.Options.BatchSize $$(refineTH 100),
      Config._envVar = "HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_BATCH_SIZE",
      Config._helpMessage =
        "multiplexed live queries are split into batches of the specified "
          <> "size. Default 100. "
    }

parseEnableAllowlist :: Opt.Parser Config.AllowListStatus
parseEnableAllowlist =
  bool Config.AllowListDisabled Config.AllowListEnabled
    <$> Opt.switch
      ( Opt.long "enable-allowlist"
          <> Opt.help (Config._helpMessage enableAllowlistOption)
      )

enableAllowlistOption :: Config.Option Config.AllowListStatus
enableAllowlistOption =
  Config.Option
    { Config._default = Config.AllowListDisabled,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_ALLOWLIST",
      Config._helpMessage = "Only accept allowed GraphQL queries"
    }

parseEnabledLogs :: forall impl. (Logging.EnabledLogTypes impl) => Opt.Parser (Maybe (HashSet (Logging.EngineLogType impl)))
parseEnabledLogs =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "enabled-log-types"
          <> Opt.help (Config._helpMessage (enabledLogsOption @impl))
      )

enabledLogsOption :: (Logging.EnabledLogTypes impl) => Config.Option (HashSet (Logging.EngineLogType impl))
enabledLogsOption =
  Config.Option
    { Config._default = Logging.defaultEnabledLogTypes,
      Config._envVar = "HASURA_GRAPHQL_ENABLED_LOG_TYPES",
      Config._helpMessage =
        "Comma separated list of enabled log types "
          <> "(default: "
          <> defaultLogTypes
          <> ")"
          <> "(all: "
          <> allAllowedLogTypes
          <> ")"
    }
  where
    defaultLogTypes = Text.unpack . Text.intercalate "," $ Witch.into @Text <$> HashSet.toList Logging.defaultEnabledEngineLogTypes
    allAllowedLogTypes = Text.unpack . Text.intercalate "," $ Witch.into @Text <$> Logging.userAllowedLogTypes

parseLogLevel :: Opt.Parser (Maybe Logging.LogLevel)
parseLogLevel =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "log-level"
          <> Opt.help (Config._helpMessage logLevelOption)
      )

logLevelOption :: Config.Option Logging.LogLevel
logLevelOption =
  Config.Option
    { Config._default = Logging.LevelInfo,
      Config._envVar = "HASURA_GRAPHQL_LOG_LEVEL",
      Config._helpMessage = "Server log level (default: info) (all: error, warn, info, debug)"
    }

parsePlanCacheSize :: Opt.Parser (Maybe Bounded.CacheSize)
parsePlanCacheSize =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "query-plan-cache-size"
          <> Opt.help
            ( "[DEPRECATED: value ignored.] The maximum number of query plans "
                <> "that can be cached, allowed values: 0-65535, "
                <> "0 disables the cache. Default 4000"
            )
      )

parseGraphqlDevMode :: Opt.Parser Config.DevModeStatus
parseGraphqlDevMode =
  bool Config.DevModeDisabled Config.DevModeEnabled
    <$> Opt.switch
      ( Opt.long "dev-mode"
          <> Opt.help (Config._helpMessage graphqlDevModeOption)
      )

graphqlDevModeOption :: Config.Option Config.DevModeStatus
graphqlDevModeOption =
  Config.Option
    { Config._default = Config.DevModeDisabled,
      Config._envVar = "HASURA_GRAPHQL_DEV_MODE",
      Config._helpMessage = "Set dev mode for GraphQL requests; include 'internal' key in the errors extensions (if required) of the response"
    }

parseGraphqlAdminInternalErrors :: Opt.Parser (Maybe Config.AdminInternalErrorsStatus)
parseGraphqlAdminInternalErrors =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "admin-internal-errors"
          <> Opt.help (Config._helpMessage graphqlAdminInternalErrorsOption)
      )

graphqlAdminInternalErrorsOption :: Config.Option Config.AdminInternalErrorsStatus
graphqlAdminInternalErrorsOption =
  Config.Option
    { -- Default to `true` to enable backwards compatibility
      Config._default = Config.AdminInternalErrorsEnabled,
      Config._envVar = "HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS",
      Config._helpMessage = "Enables including 'internal' information in an error response for requests made by an 'admin' (default: true)"
    }

parseGraphqlEventsHttpPoolSize :: Opt.Parser (Maybe (Refined Positive Int))
parseGraphqlEventsHttpPoolSize =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "events-http-pool-size"
          <> Opt.metavar (Config._envVar graphqlEventsHttpPoolSizeOption)
          <> Opt.help (Config._helpMessage graphqlEventsHttpPoolSizeOption)
      )

graphqlEventsHttpPoolSizeOption :: Config.Option (Refined Positive Int)
graphqlEventsHttpPoolSizeOption =
  Config.Option
    { Config._default = $$(refineTH @Positive @Int 100),
      Config._envVar = "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE",
      Config._helpMessage = "Max event processing threads (default: 100)"
    }

parseGraphqlEventsFetchInterval :: Opt.Parser (Maybe (Refined NonNegative Milliseconds))
parseGraphqlEventsFetchInterval =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "events-fetch-interval"
          <> Opt.metavar (Config._envVar graphqlEventsFetchIntervalOption)
          <> Opt.help (Config._helpMessage graphqlEventsFetchIntervalOption)
      )

graphqlEventsFetchIntervalOption :: Config.Option (Refined NonNegative Milliseconds)
graphqlEventsFetchIntervalOption =
  Config.Option
    { Config._default = $$(refineTH @NonNegative @Milliseconds 1000),
      Config._envVar = "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL",
      Config._helpMessage = "Interval in milliseconds to sleep before trying to fetch events again after a fetch returned no events from postgres (default: 1 second)."
    }

parseGraphqlAsyncActionsFetchInterval :: Opt.Parser (Maybe Config.OptionalInterval)
parseGraphqlAsyncActionsFetchInterval =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "async-actions-fetch-interval"
          <> Opt.metavar (Config._envVar asyncActionsFetchIntervalOption)
          <> Opt.help (Config._helpMessage asyncActionsFetchIntervalOption)
      )

asyncActionsFetchIntervalOption :: Config.Option Config.OptionalInterval
asyncActionsFetchIntervalOption =
  Config.Option
    { Config._default = Config.Interval $$(refineTH 1000),
      Config._envVar = "HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL",
      Config._helpMessage =
        "Interval in milliseconds to sleep before trying to fetch new async actions. "
          ++ "Value \"0\" implies completely disable fetching async actions from storage. "
          ++ "Default 1000 milliseconds"
    }

parseEnableRemoteSchemaPerms :: Opt.Parser Options.RemoteSchemaPermissions
parseEnableRemoteSchemaPerms =
  fmap (bool Options.DisableRemoteSchemaPermissions Options.EnableRemoteSchemaPermissions)
    $ Opt.switch
      ( Opt.long "enable-remote-schema-permissions"
          <> Opt.help (Config._helpMessage enableRemoteSchemaPermsOption)
      )

enableRemoteSchemaPermsOption :: Config.Option Options.RemoteSchemaPermissions
enableRemoteSchemaPermsOption =
  Config.Option
    { Config._default = Options.DisableRemoteSchemaPermissions,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS",
      Config._helpMessage = "Enables remote schema permissions (default: false)"
    }

parseWebSocketCompression :: Opt.Parser WebSockets.CompressionOptions
parseWebSocketCompression =
  bool WebSockets.NoCompression (WebSockets.PermessageDeflateCompression WebSockets.defaultPermessageDeflate)
    <$> Opt.switch
      ( Opt.long "websocket-compression"
          <> Opt.help (Config._helpMessage webSocketCompressionOption)
      )

webSocketCompressionOption :: Config.Option WebSockets.CompressionOptions
webSocketCompressionOption =
  Config.Option
    { Config._default = WebSockets.NoCompression,
      Config._envVar = "HASURA_GRAPHQL_CONNECTION_COMPRESSION",
      Config._helpMessage = "Enable WebSocket permessage-deflate compression (default: false)"
    }

parseWebSocketKeepAlive :: Opt.Parser (Maybe Config.KeepAliveDelay)
parseWebSocketKeepAlive =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "websocket-keepalive"
          <> Opt.help (Config._helpMessage webSocketKeepAliveOption)
      )

-- NOTE: this is purely used by Apollo-Subscription-Transport-WS
webSocketKeepAliveOption :: Config.Option Config.KeepAliveDelay
webSocketKeepAliveOption =
  Config.Option
    { Config._default = Config.KeepAliveDelay $$(refineTH 5),
      Config._envVar = "HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE",
      Config._helpMessage = "Control websocket keep-alive timeout (default 5 seconds)"
    }

parseInferFunctionPerms :: Opt.Parser (Maybe Options.InferFunctionPermissions)
parseInferFunctionPerms =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "infer-function-permissions"
          <> Opt.help (Config._helpMessage inferFunctionPermsOption)
      )

inferFunctionPermsOption :: Config.Option Options.InferFunctionPermissions
inferFunctionPermsOption =
  Config.Option
    { Config._default = Options.InferFunctionPermissions,
      Config._envVar = "HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS",
      Config._helpMessage = "Infers function permissions (default: true)"
    }

parseEnableMaintenanceMode :: Opt.Parser (Types.MaintenanceMode ())
parseEnableMaintenanceMode =
  fmap (bool Types.MaintenanceModeDisabled (Types.MaintenanceModeEnabled ()))
    $ Opt.switch
      ( Opt.long "enable-maintenance-mode"
          <> Opt.help (Config._helpMessage enableMaintenanceModeOption)
      )

enableMaintenanceModeOption :: Config.Option (Types.MaintenanceMode ())
enableMaintenanceModeOption =
  Config.Option
    { Config._default = Types.MaintenanceModeDisabled,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE",
      Config._helpMessage = "Flag to enable maintenance mode in the graphql-engine"
    }

parseSchemaPollInterval :: Opt.Parser (Maybe Config.OptionalInterval)
parseSchemaPollInterval =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "schema-sync-poll-interval"
          <> Opt.metavar (Config._envVar schemaPollIntervalOption)
          <> Opt.help (Config._helpMessage schemaPollIntervalOption)
      )

schemaPollIntervalOption :: Config.Option Config.OptionalInterval
schemaPollIntervalOption =
  Config.Option
    { -- 1000 Milliseconds or 1 Second
      Config._default = Config.Interval $$(refineTH 1000),
      Config._envVar = "HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL",
      Config._helpMessage = "Interval to poll metadata storage for updates in milliseconds - Default 1000 (1s) - Set to 0 to disable"
    }

parseExperimentalFeatures :: Opt.Parser (Maybe (HashSet Types.ExperimentalFeature))
parseExperimentalFeatures =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "experimental-features"
          <> Opt.help (Config._helpMessage experimentalFeaturesOption)
      )

experimentalFeaturesOption :: Config.Option (HashSet Types.ExperimentalFeature)
experimentalFeaturesOption =
  Config.Option
    { Config._default = HashSet.empty,
      Config._envVar = "HASURA_GRAPHQL_EXPERIMENTAL_FEATURES",
      Config._helpMessage =
        "Comma separated list of experimental features. (all: inherited_roles,optimize_permission_filters and naming_convention, streaming_subscriptions, apollo_federation). "
          <> "optimize_permission_filters: Use experimental SQL optimization"
          <> "transformations for permission filters. "
          <> "inherited_roles: ignored; inherited roles cannot be switched off"
          <> "naming_convention: apply naming convention (graphql-default/hasura-default) based on source customization"
          <> "apollo_federation: use hasura as a subgraph in an Apollo gateway (deprecated)"
          <> "streaming_subscriptions: A streaming subscription streams the response according to the cursor provided by the user"
    }

parseEventsFetchBatchSize :: Opt.Parser (Maybe (Refined NonNegative Int))
parseEventsFetchBatchSize =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "events-fetch-batch-size"
          <> Opt.metavar (Config._envVar eventsFetchBatchSizeOption)
          <> Opt.help (Config._helpMessage eventsFetchBatchSizeOption)
      )

eventsFetchBatchSizeOption :: Config.Option (Refined NonNegative Int)
eventsFetchBatchSizeOption =
  Config.Option
    { Config._default = $$(refineTH @NonNegative @Int 100),
      Config._envVar = "HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE",
      Config._helpMessage =
        "The maximum number of events to be fetched from the events table in a single batch. Default 100"
          ++ "Value \"0\" implies completely disable fetching events from events table. "
    }

parseGracefulShutdownTimeout :: Opt.Parser (Maybe (Refined NonNegative Seconds))
parseGracefulShutdownTimeout =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "graceful-shutdown-timeout"
          <> Opt.metavar "<INTERVAL (seconds)>"
          <> Opt.help (Config._helpMessage gracefulShutdownOption)
      )

gracefulShutdownOption :: Config.Option (Refined NonNegative Seconds)
gracefulShutdownOption =
  Config.Option
    { Config._default = $$(refineTH @NonNegative @Seconds 60),
      Config._envVar = "HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT",
      Config._helpMessage =
        "Timeout for graceful shutdown before which in-flight scheduled events, "
          <> " cron events and async actions to complete (default: 60 seconds)"
    }

parseWebSocketConnectionInitTimeout :: Opt.Parser (Maybe Config.WSConnectionInitTimeout)
parseWebSocketConnectionInitTimeout =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "websocket-connection-init-timeout"
          <> Opt.help (Config._helpMessage webSocketConnectionInitTimeoutOption)
      )

-- NOTE: this is purely used by GraphQL-WS
webSocketConnectionInitTimeoutOption :: Config.Option Config.WSConnectionInitTimeout
webSocketConnectionInitTimeoutOption =
  Config.Option
    { Config._default = Config.WSConnectionInitTimeout $$(refineTH 3),
      Config._envVar = "HASURA_GRAPHQL_WEBSOCKET_CONNECTION_INIT_TIMEOUT", -- FIXME?: maybe a better name
      Config._helpMessage = "Control websocket connection_init timeout (default 3 seconds)"
    }

parseEnableMetadataQueryLogging :: Opt.Parser Server.Logging.MetadataQueryLoggingMode
parseEnableMetadataQueryLogging =
  fmap (bool Server.Logging.MetadataQueryLoggingDisabled Server.Logging.MetadataQueryLoggingEnabled)
    $ Opt.switch
      ( Opt.long "enable-metadata-query-logging"
          <> Opt.help (Config._helpMessage enableMetadataQueryLoggingOption)
      )

enableMetadataQueryLoggingOption :: Config.Option Server.Logging.MetadataQueryLoggingMode
enableMetadataQueryLoggingOption =
  Config.Option
    { Config._default = Server.Logging.MetadataQueryLoggingDisabled,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING",
      Config._helpMessage = "Enables the query field in http-logs for metadata queries (default: false)"
    }

-- TODO(SOLOMON): The defaulting behavior for this occurs inside the Engine. In
-- an isolated PR we should move that defaulting in the parsing stage.
parseDefaultNamingConvention :: Opt.Parser (Maybe NC.NamingCase)
parseDefaultNamingConvention =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "default-naming-convention"
          <> Opt.help (Config._helpMessage defaultNamingConventionOption)
      )

defaultNamingConventionOption :: Config.Option NC.NamingCase
defaultNamingConventionOption =
  Config.Option
    { Config._default = NC.HasuraCase,
      Config._envVar = "HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION",
      Config._helpMessage =
        "Default naming convention for the auto generated graphql names. Possible values are"
          <> "hasura-default: Use snake_case for all names."
          <> "graphql-default: Use camelCase for field names and PascalCase for type names."
    }

parseExtensionsSchema :: Opt.Parser (Maybe MonadTx.ExtensionsSchema)
parseExtensionsSchema =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "metadata-database-extensions-schema"
          <> Opt.help (Config._helpMessage metadataDBExtensionsSchemaOption)
      )

metadataDefaultsOption :: Config.Option MetadataDefaults
metadataDefaultsOption =
  Config.Option
    { Config._default = emptyMetadataDefaults,
      Config._envVar = "HASURA_GRAPHQL_METADATA_DEFAULTS",
      Config._helpMessage = "Default values to be included in metadata."
    }

parseMetadataDefaults :: Opt.Parser (Maybe MetadataDefaults)
parseMetadataDefaults =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "metadata-defaults"
          <> Opt.help (Config._helpMessage metadataDefaultsOption)
      )

metadataDBExtensionsSchemaOption :: Config.Option MonadTx.ExtensionsSchema
metadataDBExtensionsSchemaOption =
  Config.Option
    { Config._default = MonadTx.ExtensionsSchema "public",
      Config._envVar = "HASURA_GRAPHQL_METADATA_DATABASE_EXTENSIONS_SCHEMA",
      Config._helpMessage =
        "Name of the schema where Hasura can install database extensions. Default: public"
    }

apolloFederationStatusOption :: Config.Option (Maybe Types.ApolloFederationStatus)
apolloFederationStatusOption =
  Config.Option
    { Config._default = Nothing,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_APOLLO_FEDERATION",
      Config._helpMessage = "Enable Apollo Federation (default: false). This will allow hasura to be used as a subgraph in an Apollo gateway"
    }

parseApolloFederationStatus :: Opt.Parser (Maybe Types.ApolloFederationStatus)
parseApolloFederationStatus =
  (bool Nothing (Just Types.ApolloFederationEnabled))
    <$> Opt.switch
      ( Opt.long "enable-apollo-federation"
          <> Opt.help (Config._helpMessage apolloFederationStatusOption)
      )

closeWebsocketsOnMetadataChangeOption :: Config.Option (Types.CloseWebsocketsOnMetadataChangeStatus)
closeWebsocketsOnMetadataChangeOption =
  Config.Option
    { Config._default = Types.CWMCEnabled,
      Config._envVar = "HASURA_GRAPHQL_CLOSE_WEBSOCKETS_ON_METADATA_CHANGE",
      Config._helpMessage = "Close all the websocket connections (with error code 1012) on metadata change (default: true)."
    }

parseEnableCloseWebsocketsOnMetadataChange :: Opt.Parser (Maybe Types.CloseWebsocketsOnMetadataChangeStatus)
parseEnableCloseWebsocketsOnMetadataChange =
  (bool Nothing (Just Types.CWMCDisabled))
    <$> Opt.switch
      ( Opt.long "disable-close-websockets-on-metadata-change"
          <> Opt.help (Config._helpMessage closeWebsocketsOnMetadataChangeOption)
      )

parseMaxTotalHeaderLength :: Opt.Parser (Maybe Int)
parseMaxTotalHeaderLength =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "max-total-header-length"
          <> Opt.help (Config._helpMessage maxTotalHeaderLengthOption)
      )

maxTotalHeaderLengthOption :: Config.Option Int
maxTotalHeaderLengthOption =
  Config.Option
    { Config._default = (1024 * 1024),
      Config._envVar = "HASURA_GRAPHQL_MAX_TOTAL_HEADER_LENGTH",
      Config._helpMessage = "Max cumulative length of all headers in bytes (Default: 1MB)"
    }

triggersErrorLogLevelStatusOption :: Config.Option (Types.TriggersErrorLogLevelStatus)
triggersErrorLogLevelStatusOption =
  Config.Option
    { Config._default = Types.TriggersErrorLogLevelDisabled,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_TRIGGERS_ERROR_LOG_LEVEL",
      Config._helpMessage = "Set log-level as error for Trigger error logs (Event Triggers, Scheduled Triggers, Cron Triggers) (default: false)."
    }

parseTriggersErrorLoglevelStatus :: Opt.Parser (Maybe Types.TriggersErrorLogLevelStatus)
parseTriggersErrorLoglevelStatus =
  (bool Nothing (Just Types.TriggersErrorLogLevelEnabled))
    <$> Opt.switch
      ( Opt.long "enable-triggers-error-log-level"
          <> Opt.help (Config._helpMessage triggersErrorLogLevelStatusOption)
      )

asyncActionsFetchBatchSizeOption :: Config.Option Int
asyncActionsFetchBatchSizeOption =
  Config.Option
    { Config._default = 10,
      Config._envVar = "HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_BATCH_SIZE",
      Config._helpMessage = "Number of requests processed at a time in asynchronous actions (Default: 10)"
    }

parseAsyncActionsFetchBatchSize :: Opt.Parser (Maybe Int)
parseAsyncActionsFetchBatchSize =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "async-actions-fetch-batch-size"
          <> Opt.help (Config._helpMessage asyncActionsFetchBatchSizeOption)
      )

persistedQueriesOption :: Config.Option (Types.PersistedQueriesState)
persistedQueriesOption =
  Config.Option
    { Config._default = Types.PersistedQueriesDisabled,
      Config._envVar = "HASURA_GRAPHQL_ENABLE_PERSISTED_QUERIES",
      Config._helpMessage = "Enable automated persisted queries (default: false)."
    }

parsePersistedQueries :: Opt.Parser (Maybe Types.PersistedQueriesState)
parsePersistedQueries =
  (bool Nothing (Just Types.PersistedQueriesEnabled))
    <$> Opt.switch
      ( Opt.long "enable-persisted-queries"
          <> Opt.help (Config._helpMessage persistedQueriesOption)
      )

persistedQueriesTtlOption :: Config.Option Int
persistedQueriesTtlOption =
  Config.Option
    { Config._default = 5,
      Config._envVar = "HASURA_GRAPHQL_PERSISTED_QUERIES_TTL",
      Config._helpMessage = "TTL for queries in the cache store (default: 5 seconds)."
    }

parsePersistedQueriesTtl :: Opt.Parser (Maybe Int)
parsePersistedQueriesTtl =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "persisted-queries-ttl"
          <> Opt.help (Config._helpMessage persistedQueriesTtlOption)
      )

--------------------------------------------------------------------------------
-- Pretty Printer

serveCmdFooter :: PP.Doc
serveCmdFooter =
  examplesDoc PP.<$> PP.text "" PP.<$> envVarDoc
  where
    examplesDoc = PP.mkExamplesDoc examples
    examples =
      [ [ "# Start GraphQL Engine on default port (8080) with console enabled",
          "graphql-engine --database-url <database-url> serve --enable-console"
        ],
        [ "# Start GraphQL Engine on default port (8080) with console disabled",
          "graphql-engine --database-url <database-url> serve"
        ],
        [ "# Start GraphQL Engine on a different port (say 9090) with console disabled",
          "graphql-engine --database-url <database-url> serve --server-port 9090"
        ],
        [ "# Start GraphQL Engine with admin secret key",
          "graphql-engine --database-url <database-url> serve --admin-secret <adminsecretkey>"
        ],
        [ "# Start GraphQL Engine with restrictive CORS policy (only allow https://example.com:8080)",
          "graphql-engine --database-url <database-url> serve --cors-domain https://example.com:8080"
        ],
        [ "# Start GraphQL Engine with multiple domains for CORS (https://example.com, http://localhost:3000 and https://*.foo.bar.com)",
          "graphql-engine --database-url <database-url> serve --cors-domain \"https://example.com, https://*.foo.bar.com, http://localhost:3000\""
        ],
        [ "# Start GraphQL Engine with Authentication Webhook (GET)",
          "graphql-engine --database-url <database-url> serve --admin-secret <adminsecretkey>"
            <> " --auth-hook https://mywebhook.com/get"
        ],
        [ "# Start GraphQL Engine with Authentication Webhook (POST)",
          "graphql-engine --database-url <database-url> serve --admin-secret <adminsecretkey>"
            <> " --auth-hook https://mywebhook.com/post --auth-hook-mode POST"
        ],
        [ "# Start GraphQL Engine with telemetry enabled/disabled",
          "graphql-engine --database-url <database-url> serve --enable-telemetry true|false"
        ],
        [ "# Start GraphQL Engine with HTTP compression enabled for '/v1/query' and '/v1/graphql' endpoints",
          "graphql-engine --database-url <database-url> serve --enable-compression"
        ],
        [ "# Start GraphQL Engine with enable/disable including 'internal' information in an error response for the request made by an 'admin'",
          "graphql-engine --database-url <database-url> serve --admin-internal-errors true|false"
        ]
      ]

    envVarDoc = PP.mkEnvVarDoc $ envVars <> eventEnvs
    envVars =
      [ Config.optionPP servePortOption,
        Config.optionPP serveHostOption,
        Config.optionPP pgStripesOption,
        Config.optionPP pgConnsOption,
        Config.optionPP pgTimeoutOption,
        Config.optionPP pgConnLifetimeOption,
        Config.optionPP pgUsePreparedStatementsOption,
        Config.optionPP pgPoolTimeoutOption,
        Config.optionPP txIsolationOption,
        Config.optionPP adminSecretOption,
        Config.optionPP accessKeyOption,
        Config.optionPP authHookOption,
        Config.optionPP authHookModeOption,
        Config.optionPP authHookSendRequestBodyOption,
        Config.optionPP jwtSecretOption,
        Config.optionPP unAuthRoleOption,
        Config.optionPP corsDomainOption,
        Config.optionPP disableCorsOption,
        Config.optionPP enableConsoleOption,
        Config.optionPP consoleAssetsDirOption,
        Config.optionPP enableTelemetryOption,
        Config.optionPP wsReadCookieOption,
        Config.optionPP stringifyNumOption,
        Config.optionPP dangerousBooleanCollapseOption,
        Config.optionPP enabledAPIsOption,
        Config.optionPP mxRefetchDelayOption,
        Config.optionPP mxBatchSizeOption,
        Config.optionPP streamingMxRefetchDelayOption,
        Config.optionPP streamingMxBatchSizeOption,
        Config.optionPP enableAllowlistOption,
        Config.optionPP (enabledLogsOption @Logging.Hasura),
        Config.optionPP logLevelOption,
        Config.optionPP graphqlDevModeOption,
        Config.optionPP graphqlAdminInternalErrorsOption,
        Config.optionPP graphqlEventsHttpPoolSizeOption,
        Config.optionPP graphqlEventsFetchIntervalOption,
        Config.optionPP asyncActionsFetchIntervalOption,
        Config.optionPP enableRemoteSchemaPermsOption,
        Config.optionPP webSocketCompressionOption,
        Config.optionPP webSocketKeepAliveOption,
        Config.optionPP inferFunctionPermsOption,
        Config.optionPP enableMaintenanceModeOption,
        Config.optionPP schemaPollIntervalOption,
        Config.optionPP experimentalFeaturesOption,
        Config.optionPP eventsFetchBatchSizeOption,
        Config.optionPP gracefulShutdownOption,
        Config.optionPP webSocketConnectionInitTimeoutOption,
        Config.optionPP enableMetadataQueryLoggingOption,
        Config.optionPP defaultNamingConventionOption,
        Config.optionPP metadataDBExtensionsSchemaOption,
        Config.optionPP apolloFederationStatusOption,
        Config.optionPP closeWebsocketsOnMetadataChangeOption,
        Config.optionPP maxTotalHeaderLengthOption,
        Config.optionPP remoteNullForwardingPolicyOption,
        Config.optionPP triggersErrorLogLevelStatusOption,
        Config.optionPP asyncActionsFetchBatchSizeOption,
        Config.optionPP persistedQueriesOption,
        Config.optionPP persistedQueriesTtlOption
      ]
    eventEnvs = [Config.optionPP graphqlEventsHttpPoolSizeOption, Config.optionPP graphqlEventsFetchIntervalOption]
