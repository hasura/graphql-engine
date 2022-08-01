-- | The Arg Parser for the 'serve' subcommand.
module Hasura.Server.Init.Arg.Command.Serve
  ( module Hasura.Server.Init.Arg.Command.Serve,
  )
where

-- TODO(SOLOMON): How do I want to handle exports of the help tuples?
--  ( serveCommandParser,
--    serveCmdFooter,
--  )

--------------------------------------------------------------------------------

import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Time qualified as Time
import Database.PG.Query qualified as Query
import Hasura.Cache.Bounded qualified as Cache
import Hasura.GraphQL.Execute.Subscription.Options qualified as ESO
import Hasura.GraphQL.Schema.NamingCase qualified as NC
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as Common
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init.Arg.PrettyPrinter qualified as PP
import Hasura.Server.Init.Config (OptionalInterval)
import Hasura.Server.Init.Config qualified as Config
import Hasura.Server.Init.Env qualified as Env
import Hasura.Server.Logging qualified as Logging
import Hasura.Server.Types qualified as Types
import Hasura.Session qualified as Session
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative qualified as Opt
import Witch qualified

--------------------------------------------------------------------------------
-- Serve Command

serveCommandParser :: L.EnabledLogTypes impl => Opt.Parser (Config.ServeOptionsRaw impl)
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
    <*> parseEnableTelemetry
    <*> parseWsReadCookie
    <*> parseStringifyNum
    <*> parseDangerousBooleanCollapse
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

--------------------------------------------------------------------------------
-- Serve Options

-- TODO(SOLOMON): Review, currently accepts negative integers
parseServerPort :: Opt.Parser (Maybe Int)
parseServerPort =
  Opt.optional $
    Opt.option
      Opt.auto
      ( Opt.long "server-port"
          <> Opt.metavar "<PORT>"
          <> Opt.help (snd servePortEnv)
      )

servePortEnv :: (String, String)
servePortEnv =
  ( "HASURA_GRAPHQL_SERVER_PORT",
    "Port on which graphql-engine should be served (default: 8080)"
  )

parseServerHost :: Opt.Parser (Maybe Warp.HostPreference)
parseServerHost =
  Opt.optional $
    Opt.strOption
      ( Opt.long "server-host"
          <> Opt.metavar "<HOST>"
          <> Opt.help (snd serveHostEnv)
      )

serveHostEnv :: (String, String)
serveHostEnv =
  ( "HASURA_GRAPHQL_SERVER_HOST",
    "Host on which graphql-engine will listen (default: *)"
  )

parseConnParams :: Opt.Parser Config.ConnParamsRaw
parseConnParams =
  Config.ConnParamsRaw <$> pgStripes <*> pgConns <*> pgIdleTimeout <*> pgConnLifetime <*> pgAllowPrepare <*> pgPoolTimeout
  where
    -- TODO(SOLOMON): Review, currently accepts negative integers
    pgStripes =
      Opt.optional $
        Opt.option
          Opt.auto
          ( Opt.long "stripes"
              <> Opt.short 's'
              <> Opt.metavar "<NO OF STRIPES>"
              <> Opt.help (snd pgStripesEnv)
          )

    -- TODO(SOLOMON): Review, currently accepts negative integers
    pgConns =
      Opt.optional $
        Opt.option
          Opt.auto
          ( Opt.long "connections"
              <> Opt.short 'c'
              <> Opt.metavar "<NO OF CONNS>"
              <> Opt.help (snd pgConnsEnv)
          )

    -- TODO(SOLOMON): Review, currently accepts negative integers
    pgIdleTimeout =
      Opt.optional $
        Opt.option
          Opt.auto
          ( Opt.long "timeout"
              <> Opt.metavar "<SECONDS>"
              <> Opt.help (snd pgTimeoutEnv)
          )

    -- TODO(SOLOMON): Review, currently accepts negative integers
    pgConnLifetime =
      fmap (fmap (realToFrac :: Int -> Time.NominalDiffTime)) $
        Opt.optional $
          Opt.option
            Opt.auto
            ( Opt.long "conn-lifetime"
                <> Opt.metavar "<SECONDS>"
                <> Opt.help (snd pgConnLifetimeEnv)
            )

    pgAllowPrepare =
      Opt.optional $
        Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "use-prepared-statements"
              <> Opt.metavar "<true|false>"
              <> Opt.help (snd pgUsePrepareEnv)
          )

    -- TODO(SOLOMON): Review, currently accepts negative integers
    pgPoolTimeout =
      fmap (fmap (realToFrac :: Int -> Time.NominalDiffTime)) $
        Opt.optional $
          Opt.option
            Opt.auto
            ( Opt.long "pool-timeout"
                <> Opt.metavar "<SECONDS>"
                <> Opt.help (snd pgPoolTimeoutEnv)
            )

pgStripesEnv :: (String, String)
pgStripesEnv =
  ( "HASURA_GRAPHQL_PG_STRIPES",
    "Number of stripes (distinct sub-pools) to maintain with Postgres (default: 1). "
      <> "New connections will be taken from a particular stripe pseudo-randomly."
  )

pgConnsEnv :: (String, String)
pgConnsEnv =
  ( "HASURA_GRAPHQL_PG_CONNECTIONS",
    "Maximum number of Postgres connections that can be opened per stripe (default: 50). "
      <> "When the maximum is reached we will block until a new connection becomes available, "
      <> "even if there is capacity in other stripes."
  )

pgTimeoutEnv :: (String, String)
pgTimeoutEnv =
  ( "HASURA_GRAPHQL_PG_TIMEOUT",
    "Each connection's idle time before it is closed (default: 180 sec)"
  )

pgConnLifetimeEnv :: (String, String)
pgConnLifetimeEnv =
  ( "HASURA_GRAPHQL_PG_CONN_LIFETIME",
    "Time from connection creation after which the connection should be destroyed and a new one "
      <> "created. A value of 0 indicates we should never destroy an active connection. If 0 is "
      <> "passed, memory from large query results may not be reclaimed. (default: 600 sec)"
  )

pgUsePrepareEnv :: (String, String)
pgUsePrepareEnv =
  ( "HASURA_GRAPHQL_USE_PREPARED_STATEMENTS",
    "Use prepared statements for queries (default: true)"
  )

pgPoolTimeoutEnv :: (String, String)
pgPoolTimeoutEnv =
  ( "HASURA_GRAPHQL_PG_POOL_TIMEOUT",
    "How long to wait when acquiring a Postgres connection, in seconds (default: forever)."
  )

parseTxIsolation :: Opt.Parser (Maybe Query.TxIsolation)
parseTxIsolation =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "tx-iso"
          <> Opt.short 'i'
          <> Opt.metavar "<TXISO>"
          <> Opt.help (snd txIsolationEnv)
      )

txIsolationEnv :: (String, String)
txIsolationEnv =
  ( "HASURA_GRAPHQL_TX_ISOLATION",
    "transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)"
  )

parseAdminSecret :: Opt.Parser (Maybe Auth.AdminSecretHash)
parseAdminSecret =
  Opt.optional $
    Auth.hashAdminSecret
      <$> Opt.strOption
        ( Opt.long "admin-secret"
            <> Opt.metavar "ADMIN SECRET KEY"
            <> Opt.help (snd adminSecretEnv)
        )

adminSecretEnv :: (String, String)
adminSecretEnv =
  ( "HASURA_GRAPHQL_ADMIN_SECRET",
    "Admin Secret key, required to access this instance"
  )

parseAccessKey :: Opt.Parser (Maybe Auth.AdminSecretHash)
parseAccessKey =
  Opt.optional $
    Auth.hashAdminSecret
      <$> Opt.strOption
        ( Opt.long "access-key"
            <> Opt.metavar "ADMIN SECRET KEY (DEPRECATED: USE --admin-secret)"
            <> Opt.help (snd accessKeyEnv)
        )

accessKeyEnv :: (String, String)
accessKeyEnv =
  ( "HASURA_GRAPHQL_ACCESS_KEY",
    "Admin secret key, required to access this instance (deprecated: use HASURA_GRAPHQL_ADMIN_SECRET instead)"
  )

parseAuthHook :: Opt.Parser Config.AuthHookRaw
parseAuthHook =
  Auth.AuthHookG <$> url <*> urlType
  where
    url =
      Opt.optional $
        Opt.strOption
          ( Opt.long "auth-hook"
              <> Opt.metavar "<WEB HOOK URL>"
              <> Opt.help (snd authHookEnv)
          )
    urlType =
      Opt.optional $
        Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "auth-hook-mode"
              <> Opt.metavar "<GET|POST>"
              <> Opt.help (snd authHookModeEnv)
          )

authHookEnv :: (String, String)
authHookEnv =
  ( "HASURA_GRAPHQL_AUTH_HOOK",
    "URL of the authorization webhook required to authorize requests"
  )

authHookModeEnv :: (String, String)
authHookModeEnv =
  ( "HASURA_GRAPHQL_AUTH_HOOK_MODE",
    "HTTP method to use for authorization webhook (default: GET)"
  )

parseJwtSecret :: Opt.Parser (Maybe Auth.JWTConfig)
parseJwtSecret =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "jwt-secret"
          <> Opt.metavar "<JSON CONFIG>"
          <> Opt.help (snd jwtSecretEnv)
      )

jwtSecretEnv :: (String, String)
jwtSecretEnv =
  ( "HASURA_GRAPHQL_JWT_SECRET",
    "The JSON containing type and the JWK used for verifying. e.g: "
      <> "`{\"type\": \"HS256\", \"key\": \"<your-hmac-shared-secret>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`,"
      <> "`{\"type\": \"RS256\", \"key\": \"<your-PEM-RSA-public-key>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`"
  )

parseUnAuthRole :: Opt.Parser (Maybe Session.RoleName)
parseUnAuthRole =
  fmap mkRoleName $
    Opt.optional $
      Opt.strOption
        ( Opt.long "unauthorized-role"
            <> Opt.metavar "<ROLE>"
            <> Opt.help (snd unAuthRoleEnv)
        )
  where
    mkRoleName mText = mText >>= Session.mkRoleName

unAuthRoleEnv :: (String, String)
unAuthRoleEnv =
  ( "HASURA_GRAPHQL_UNAUTHORIZED_ROLE",
    "Unauthorized role, used when admin-secret is not sent in admin-secret only mode "
      ++ "or \"Authorization\" header is absent in JWT mode"
  )

parseCorsConfig :: Opt.Parser (Maybe Cors.CorsConfig)
parseCorsConfig = mapCC <$> disableCors <*> corsDomain
  where
    corsDomain =
      Opt.optional $
        Opt.option
          (Opt.eitherReader Env.fromEnv)
          ( Opt.long "cors-domain"
              <> Opt.metavar "<DOMAINS>"
              <> Opt.help (snd corsDomainEnv)
          )

    disableCors =
      Opt.switch
        ( Opt.long "disable-cors"
            <> Opt.help (snd disableCorsEnv)
        )

    mapCC isDisabled domains =
      bool domains (Just $ Cors.CCDisabled False) isDisabled

corsDomainEnv :: (String, String)
corsDomainEnv =
  ( "HASURA_GRAPHQL_CORS_DOMAIN",
    "CSV of list of domains, excluding scheme (http/https) and including  port, "
      ++ "to allow CORS for. Wildcard domains are allowed. See docs for details."
  )

disableCorsEnv :: (String, String)
disableCorsEnv =
  ( "HASURA_GRAPHQL_DISABLE_CORS",
    "Disable CORS. Do not send any CORS headers on any request"
  )

parseEnableConsole :: Opt.Parser Bool
parseEnableConsole =
  Opt.switch
    ( Opt.long "enable-console"
        <> Opt.help (snd enableConsoleEnv)
    )

enableConsoleEnv :: (String, String)
enableConsoleEnv =
  ( "HASURA_GRAPHQL_ENABLE_CONSOLE",
    "Enable API Console (default: false)"
  )

parseConsoleAssetsDir :: Opt.Parser (Maybe Text)
parseConsoleAssetsDir =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "console-assets-dir"
          <> Opt.help (snd consoleAssetsDirEnv)
      )

consoleAssetsDirEnv :: (String, String)
consoleAssetsDirEnv =
  ( "HASURA_GRAPHQL_CONSOLE_ASSETS_DIR",
    "A directory from which static assets required for console is served at"
      ++ "'/console/assets' path. Can be set to '/srv/console-assets' on the"
      ++ " default docker image to disable loading assets from CDN."
  )

-- NOTE: Should this be an 'Opt.flag'?
parseEnableTelemetry :: Opt.Parser (Maybe Bool)
parseEnableTelemetry =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "enable-telemetry"
          <> Opt.help (snd enableTelemetryEnv)
      )

enableTelemetryEnv :: (String, String)
enableTelemetryEnv =
  ( "HASURA_GRAPHQL_ENABLE_TELEMETRY",
    -- TODO (from master): better description
    "Enable anonymous telemetry (default: true)"
  )

parseWsReadCookie :: Opt.Parser Bool
parseWsReadCookie =
  Opt.switch
    ( Opt.long "ws-read-cookie"
        <> Opt.help (snd wsReadCookieEnv)
    )

wsReadCookieEnv :: (String, String)
wsReadCookieEnv =
  ( "HASURA_GRAPHQL_WS_READ_COOKIE",
    "Read cookie on WebSocket initial handshake, even when CORS is disabled."
      ++ " This can be a potential security flaw! Please make sure you know "
      ++ "what you're doing."
      ++ " This configuration is only applicable when CORS is disabled."
  )

parseStringifyNum :: Opt.Parser Options.StringifyNumbers
parseStringifyNum =
  fmap (bool Options.Don'tStringifyNumbers Options.StringifyNumbers) $
    Opt.switch
      ( Opt.long "stringify-numeric-types"
          <> Opt.help (snd stringifyNumEnv)
      )

stringifyNumEnv :: (String, String)
stringifyNumEnv =
  ( "HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES",
    "Stringify numeric types (default: false)"
  )

parseDangerousBooleanCollapse :: Opt.Parser (Maybe Bool)
parseDangerousBooleanCollapse =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "v1-boolean-null-collapse"
          <> Opt.help (snd dangerousBooleanCollapseEnv)
      )

dangerousBooleanCollapseEnv :: (String, String)
dangerousBooleanCollapseEnv =
  ( "HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE",
    "Emulate V1's behaviour re. boolean expression, where an explicit 'null'"
      <> " value will be interpreted to mean that the field should be ignored"
      <> " [DEPRECATED, WILL BE REMOVED SOON] (default: false)"
  )

parseEnabledAPIs :: Opt.Parser (Maybe [Config.API])
parseEnabledAPIs =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "enabled-apis"
          <> Opt.help (snd enabledAPIsEnv)
      )

enabledAPIsEnv :: (String, String)
enabledAPIsEnv =
  ( "HASURA_GRAPHQL_ENABLED_APIS",
    "Comma separated list of enabled APIs. (default: metadata,graphql,pgdump,config)"
  )

parseMxRefetchDelay :: Opt.Parser (Maybe ESO.RefetchInterval)
parseMxRefetchDelay =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "live-queries-multiplexed-refetch-interval"
          <> Opt.metavar "<INTERVAL(ms)>"
          <> Opt.help (snd mxRefetchDelayEnv)
      )

mxRefetchDelayEnv :: (String, String)
mxRefetchDelayEnv =
  ( "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL",
    "results will only be sent once in this interval (in milliseconds) for "
      <> "live queries which can be multiplexed. Default: 1000 (1sec)"
  )

parseMxBatchSize :: Opt.Parser (Maybe ESO.BatchSize)
parseMxBatchSize =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "live-queries-multiplexed-batch-size"
          <> Opt.metavar "BATCH_SIZE"
          <> Opt.help (snd mxBatchSizeEnv)
      )

mxBatchSizeEnv :: (String, String)
mxBatchSizeEnv =
  ( "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE",
    "multiplexed live queries are split into batches of the specified "
      <> "size. Default 100. "
  )

parseStreamingMxRefetchDelay :: Opt.Parser (Maybe ESO.RefetchInterval)
parseStreamingMxRefetchDelay =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "streaming-queries-multiplexed-refetch-interval"
          <> Opt.metavar "<INTERVAL(ms)>"
          <> Opt.help (snd streamingMxRefetchDelayEnv)
      )

streamingMxRefetchDelayEnv :: (String, String)
streamingMxRefetchDelayEnv =
  ( "HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_REFETCH_INTERVAL",
    "results will only be sent once in this interval (in milliseconds) for "
      <> "streaming queries which can be multiplexed. Default: 1000 (1sec)"
  )

parseStreamingMxBatchSize :: Opt.Parser (Maybe ESO.BatchSize)
parseStreamingMxBatchSize =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "streaming-queries-multiplexed-batch-size"
          <> Opt.metavar "BATCH_SIZE"
          <> Opt.help (snd streamingMxBatchSizeEnv)
      )

streamingMxBatchSizeEnv :: (String, String)
streamingMxBatchSizeEnv =
  ( "HASURA_GRAPHQL_STREAMING_QUERIES_MULTIPLEXED_BATCH_SIZE",
    "multiplexed live queries are split into batches of the specified "
      <> "size. Default 100. "
  )

parseEnableAllowlist :: Opt.Parser Bool
parseEnableAllowlist =
  Opt.switch
    ( Opt.long "enable-allowlist"
        <> Opt.help (snd enableAllowlistEnv)
    )

enableAllowlistEnv :: (String, String)
enableAllowlistEnv =
  ( "HASURA_GRAPHQL_ENABLE_ALLOWLIST",
    "Only accept allowed GraphQL queries"
  )

parseEnabledLogs :: L.EnabledLogTypes impl => Opt.Parser (Maybe [L.EngineLogType impl])
parseEnabledLogs =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "enabled-log-types"
          <> Opt.help (snd enabledLogsEnv)
      )

enabledLogsEnv :: (String, String)
enabledLogsEnv =
  ( "HASURA_GRAPHQL_ENABLED_LOG_TYPES",
    "Comma separated list of enabled log types "
      <> "(default: "
      <> defaultLogTypes
      <> ")"
      <> "(all: "
      <> allAllowedLogTypes
      <> ")"
  )
  where
    defaultLogTypes = T.unpack . T.intercalate "," $ Witch.into @Text <$> Set.toList L.defaultEnabledEngineLogTypes
    allAllowedLogTypes = T.unpack . T.intercalate "," $ Witch.into @Text <$> L.userAllowedLogTypes

parseLogLevel :: Opt.Parser (Maybe L.LogLevel)
parseLogLevel =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "log-level"
          <> Opt.help (snd logLevelEnv)
      )

logLevelEnv :: (String, String)
logLevelEnv =
  ( "HASURA_GRAPHQL_LOG_LEVEL",
    "Server log level (default: info) (all: error, warn, info, debug)"
  )

parsePlanCacheSize :: Opt.Parser (Maybe Cache.CacheSize)
parsePlanCacheSize =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "query-plan-cache-size"
          <> Opt.help
            ( "[DEPRECATED: value ignored.] The maximum number of query plans "
                <> "that can be cached, allowed values: 0-65535, "
                <> "0 disables the cache. Default 4000"
            )
      )

parseGraphqlDevMode :: Opt.Parser Bool
parseGraphqlDevMode =
  Opt.switch
    ( Opt.long "dev-mode"
        <> Opt.help (snd graphqlDevModeEnv)
    )

graphqlDevModeEnv :: (String, String)
graphqlDevModeEnv =
  ( "HASURA_GRAPHQL_DEV_MODE",
    "Set dev mode for GraphQL requests; include 'internal' key in the errors extensions (if required) of the response"
  )

parseGraphqlAdminInternalErrors :: Opt.Parser (Maybe Bool)
parseGraphqlAdminInternalErrors =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "admin-internal-errors"
          <> Opt.help (snd graphqlAdminInternalErrorsEnv)
      )

graphqlAdminInternalErrorsEnv :: (String, String)
graphqlAdminInternalErrorsEnv =
  ( "HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS",
    "Enables including 'internal' information in an error response for requests made by an 'admin' (default: true)"
  )

-- TODO(SOLOMON): Review, currently accepts negative integers
parseGraphqlEventsHttpPoolSize :: Opt.Parser (Maybe Int)
parseGraphqlEventsHttpPoolSize =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "events-http-pool-size"
          <> Opt.metavar (fst graphqlEventsHttpPoolSizeEnv)
          <> Opt.help (snd graphqlEventsHttpPoolSizeEnv)
      )

-- TODO(SOLOMON): There actually isn't a default value set in 'ServeOptions' for this:
graphqlEventsHttpPoolSizeEnv :: (String, String)
graphqlEventsHttpPoolSizeEnv =
  ( "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE",
    "Max event processing threads (default: 100)"
  )

-- TODO(SOLOMON): Review, currently accepts negative integers
parseGraphqlEventsFetchInterval :: Opt.Parser (Maybe Milliseconds)
parseGraphqlEventsFetchInterval =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "events-fetch-interval"
          <> Opt.metavar (fst graphqlEventsFetchIntervalEnv)
          <> Opt.help (snd graphqlEventsFetchIntervalEnv)
      )

graphqlEventsFetchIntervalEnv :: (String, String)
graphqlEventsFetchIntervalEnv =
  ( "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL",
    "Interval in milliseconds to sleep before trying to fetch events again after a fetch returned no events from postgres."
  )

-- TODO(SOLOMON): Review, currently accepts negative integers
parseGraphqlAsyncActionsFetchInterval :: Opt.Parser (Maybe Config.OptionalInterval)
parseGraphqlAsyncActionsFetchInterval =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "async-actions-fetch-interval"
          <> Opt.metavar (fst asyncActionsFetchIntervalEnv)
          <> Opt.help (snd asyncActionsFetchIntervalEnv)
      )

asyncActionsFetchIntervalEnv :: (String, String)
asyncActionsFetchIntervalEnv =
  ( "HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL",
    "Interval in milliseconds to sleep before trying to fetch new async actions. "
      ++ "Value \"0\" implies completely disable fetching async actions from storage. "
      ++ "Default 1000 milliseconds"
  )

parseEnableRemoteSchemaPerms :: Opt.Parser Options.RemoteSchemaPermissions
parseEnableRemoteSchemaPerms =
  fmap (bool Options.DisableRemoteSchemaPermissions Options.EnableRemoteSchemaPermissions) $
    Opt.switch
      ( Opt.long "enable-remote-schema-permissions"
          <> Opt.help (snd enableRemoteSchemaPermsEnv)
      )

enableRemoteSchemaPermsEnv :: (String, String)
enableRemoteSchemaPermsEnv =
  ( "HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS",
    "Enables remote schema permissions (default: false)"
  )

parseWebSocketCompression :: Opt.Parser Bool
parseWebSocketCompression =
  Opt.switch
    ( Opt.long "websocket-compression"
        <> Opt.help (snd webSocketCompressionEnv)
    )

webSocketCompressionEnv :: (String, String)
webSocketCompressionEnv =
  ( "HASURA_GRAPHQL_CONNECTION_COMPRESSION",
    "Enable WebSocket permessage-deflate compression (default: false)"
  )

-- TODO(SOLOMON): Review, currently accepts negative integers
parseWebSocketKeepAlive :: Opt.Parser (Maybe Config.KeepAliveDelay)
parseWebSocketKeepAlive =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "websocket-keepalive"
          <> Opt.help (snd webSocketKeepAliveEnv)
      )

-- NOTE: this is purely used by Apollo-Subscription-Transport-WS
webSocketKeepAliveEnv :: (String, String)
webSocketKeepAliveEnv =
  ( "HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE",
    "Control websocket keep-alive timeout (default 5 seconds)"
  )

parseInferFunctionPerms :: Opt.Parser (Maybe Options.InferFunctionPermissions)
parseInferFunctionPerms =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "infer-function-permissions"
          <> Opt.help (snd inferFunctionPermsEnv)
      )

inferFunctionPermsEnv :: (String, String)
inferFunctionPermsEnv =
  ( "HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS",
    "Infers function permissions (default: true)"
  )

parseEnableMaintenanceMode :: Opt.Parser (Types.MaintenanceMode ())
parseEnableMaintenanceMode =
  fmap (bool Types.MaintenanceModeDisabled (Types.MaintenanceModeEnabled ())) $
    Opt.switch
      ( Opt.long "enable-maintenance-mode"
          <> Opt.help (snd enableMaintenanceModeEnv)
      )

enableMaintenanceModeEnv :: (String, String)
enableMaintenanceModeEnv =
  ( "HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE",
    "Flag to enable maintenance mode in the graphql-engine"
  )

-- TODO(SOLOMON): Review, currently accepts negative integers
parseSchemaPollInterval :: Opt.Parser (Maybe OptionalInterval)
parseSchemaPollInterval =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "schema-sync-poll-interval"
          <> Opt.metavar (fst schemaPollIntervalEnv)
          <> Opt.help (snd schemaPollIntervalEnv)
      )

schemaPollIntervalEnv :: (String, String)
schemaPollIntervalEnv =
  ( "HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL",
    "Interval to poll metadata storage for updates in milliseconds - Default 1000 (1s) - Set to 0 to disable"
  )

parseExperimentalFeatures :: Opt.Parser (Maybe [Types.ExperimentalFeature])
parseExperimentalFeatures =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "experimental-features"
          <> Opt.help (snd experimentalFeaturesEnv)
      )

experimentalFeaturesEnv :: (String, String)
experimentalFeaturesEnv =
  ( "HASURA_GRAPHQL_EXPERIMENTAL_FEATURES",
    "Comma separated list of experimental features. (all: inherited_roles,optimize_permission_filters and naming_convention, streaming_subscriptions, apollo_federation). "
      <> "optimize_permission_filters: Use experimental SQL optimization"
      <> "transformations for permission filters. "
      <> "inherited_roles: ignored; inherited roles cannot be switched off"
      <> "naming_convention: apply naming convention (graphql-default/hasura-default) based on source customization"
      <> "apollo_federation: use hasura as a subgraph in an Apollo gateway"
      -- TODO(SOLOMON): Write a description of this experimental feature:
      -- <> "streaming_subscriptions: ..."
  )

parseEventsFetchBatchSize :: Opt.Parser (Maybe Common.NonNegativeInt)
parseEventsFetchBatchSize =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "events-fetch-batch-size"
          <> Opt.metavar (fst eventsFetchBatchSizeEnv)
          <> Opt.help (snd eventsFetchBatchSizeEnv)
      )

eventsFetchBatchSizeEnv :: (String, String)
eventsFetchBatchSizeEnv =
  ( "HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE",
    "The maximum number of events to be fetched from the events table in a single batch. Default 100"
      ++ "Value \"0\" implies completely disable fetching events from events table. "
  )

-- TODO(SOLOMON): Review, currently accepts negative integers
parseGracefulShutdownTimeout :: Opt.Parser (Maybe Seconds)
parseGracefulShutdownTimeout =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "graceful-shutdown-timeout"
          <> Opt.metavar "<INTERVAL (seconds)>"
          <> Opt.help (snd gracefulShutdownEnv)
      )

gracefulShutdownEnv :: (String, String)
gracefulShutdownEnv =
  ( "HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT",
    "Timeout for graceful shutdown before which in-flight scheduled events, "
      <> " cron events and async actions to complete (default: 60 seconds)"
  )

-- TODO(SOLOMON): Review, currently accepts negative integers
parseWebSocketConnectionInitTimeout :: Opt.Parser (Maybe Config.WSConnectionInitTimeout)
parseWebSocketConnectionInitTimeout =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "websocket-connection-init-timeout"
          <> Opt.help (snd webSocketConnectionInitTimeoutEnv)
      )

-- NOTE: this is purely used by GraphQL-WS
webSocketConnectionInitTimeoutEnv :: (String, String)
webSocketConnectionInitTimeoutEnv =
  ( "HASURA_GRAPHQL_WEBSOCKET_CONNECTION_INIT_TIMEOUT", -- FIXME?: maybe a better name
    "Control websocket connection_init timeout (default 3 seconds)"
  )

parseEnableMetadataQueryLogging :: Opt.Parser Logging.MetadataQueryLoggingMode
parseEnableMetadataQueryLogging =
  fmap (bool Logging.MetadataQueryLoggingDisabled Logging.MetadataQueryLoggingEnabled) $
    Opt.switch
      ( Opt.long "enable-metadata-query-logging"
          <> Opt.help (snd enableMetadataQueryLoggingEnv)
      )

enableMetadataQueryLoggingEnv :: (String, String)
enableMetadataQueryLoggingEnv =
  ( "HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING",
    "Enables the query field in http-logs for metadata queries (default: false)"
  )

-- TODO(SOLOMON): Should this have a default value?
parseDefaultNamingConvention :: Opt.Parser (Maybe NC.NamingCase)
parseDefaultNamingConvention =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "default-naming-convention"
          <> Opt.help (snd defaultNamingConventionEnv)
      )

defaultNamingConventionEnv :: (String, String)
defaultNamingConventionEnv =
  ( "HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION",
    "Default naming convention for the auto generated graphql names. Possible values are"
      <> "hasura-default: Use snake_case for all names."
      <> "graphql-default: Use camelCase for field names and PascalCase for type names."
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
      [ servePortEnv,
        serveHostEnv,
        pgStripesEnv,
        pgConnsEnv,
        pgTimeoutEnv,
        pgConnLifetimeEnv,
        pgUsePrepareEnv,
        pgPoolTimeoutEnv,
        txIsolationEnv,
        adminSecretEnv,
        accessKeyEnv,
        authHookEnv,
        authHookModeEnv,
        jwtSecretEnv,
        unAuthRoleEnv,
        corsDomainEnv,
        disableCorsEnv,
        enableConsoleEnv,
        consoleAssetsDirEnv,
        enableTelemetryEnv,
        wsReadCookieEnv,
        stringifyNumEnv,
        dangerousBooleanCollapseEnv,
        enabledAPIsEnv,
        mxRefetchDelayEnv,
        mxBatchSizeEnv,
        streamingMxRefetchDelayEnv,
        streamingMxBatchSizeEnv,
        enableAllowlistEnv,
        enabledLogsEnv,
        logLevelEnv,
        graphqlDevModeEnv,
        graphqlAdminInternalErrorsEnv,
        graphqlEventsHttpPoolSizeEnv,
        graphqlEventsFetchIntervalEnv,
        asyncActionsFetchIntervalEnv,
        enableRemoteSchemaPermsEnv,
        webSocketCompressionEnv,
        webSocketKeepAliveEnv,
        inferFunctionPermsEnv,
        enableMaintenanceModeEnv,
        schemaPollIntervalEnv,
        experimentalFeaturesEnv,
        eventsFetchBatchSizeEnv,
        gracefulShutdownEnv,
        webSocketConnectionInitTimeoutEnv,
        enableMetadataQueryLoggingEnv,
        defaultNamingConventionEnv
      ]
    eventEnvs = [graphqlEventsHttpPoolSizeEnv, graphqlEventsFetchIntervalEnv]
