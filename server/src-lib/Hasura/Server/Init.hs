{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O0 #-}

-- | Types and functions related to the server initialisation
module Hasura.Server.Init
  ( module Hasura.Server.Init,
    module Hasura.Server.Init.Config,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.ByteString.Char8 (pack, unpack)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.HashSet qualified as Set
import Data.String qualified as DataString
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime)
import Data.URL.Template
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Cache.Bounded qualified as Cache (CacheSize, parseCacheSize)
import Hasura.Eventing.EventTrigger (defaultFetchBatchSize)
import Hasura.GraphQL.Execute.LiveQuery.Options qualified as LQ
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.Server.Auth
import Hasura.Server.Cors
import Hasura.Server.Init.Config
import Hasura.Server.Logging
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Session
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Types.URI
  ( Query,
    QueryItem,
    parseQuery,
    renderQuery,
  )
import Network.URI (URI (..), parseURI, uriQuery)
import Network.Wai.Handler.Warp (HostPreference)
import Network.WebSockets qualified as WS
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen qualified as PP

{- Note [ReadOnly Mode]
~~~~~~~~~~~~~~~~~~~~~~~~~

This mode starts the server in a (database) read-only mode. That is, only
read-only queries are allowed on users' database sources, and write
queries throw a runtime error. The use-case is for failsafe operations.
Metadata APIs are also disabled.

Following is the precise behaviour -
  1. For any GraphQL API (relay/hasura; http/websocket) - disable execution of
  mutations
  2. Metadata API is disabled
  3. /v2/query API - insert, delete, update, run_sql are disabled
  4. /v1/query API - insert, delete, update, run_sql are disabled
  5. No source catalog migrations are run
  6. During build schema cache phase, building event triggers are disabled (as
  they create corresponding database triggers)
-}

getDbId :: Q.TxE QErr Text
getDbId =
  runIdentity . Q.getRow
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
    SELECT (hasura_uuid :: text) FROM hdb_catalog.hdb_version
  |]
      ()
      False

getPgVersion :: Q.TxE QErr PGVersion
getPgVersion = PGVersion <$> Q.serverVersion

generateInstanceId :: IO InstanceId
generateInstanceId = InstanceId <$> generateFingerprint

data StartupTimeInfo = StartupTimeInfo
  { _stiMessage :: !Text,
    _stiTimeTaken :: !Double
  }

$(J.deriveJSON hasuraJSON ''StartupTimeInfo)

returnJust :: Monad m => a -> m (Maybe a)
returnJust = return . Just

-- | Lookup a key in the application environment then parse the value
-- with a 'FromEnv' instance'
considerEnv :: FromEnv a => String -> WithEnv (Maybe a)
considerEnv envVar = do
  env <- ask
  case lookup envVar env of
    Nothing -> return Nothing
    Just val -> either throwErr returnJust $ fromEnv val
  where
    throwErr s =
      throwError $
        "Fatal Error:- Environment variable " ++ envVar ++ ": " ++ s

-- | Lookup a list of keys with 'considerEnv' and return the first
-- value to parse successfully.
considerEnvs :: FromEnv a => [String] -> WithEnv (Maybe a)
considerEnvs envVars = foldl1 (<|>) <$> mapM considerEnv envVars

withEnv :: FromEnv a => Maybe a -> String -> WithEnv (Maybe a)
withEnv mVal envVar =
  maybe (considerEnv envVar) returnJust mVal

-- | Return 'a' if Just or else call 'considerEnv' with a list of env keys k
withEnvs :: FromEnv a => Maybe a -> [String] -> WithEnv (Maybe a)
withEnvs mVal envVars =
  maybe (considerEnvs envVars) returnJust mVal

withEnvBool :: Bool -> String -> WithEnv Bool
withEnvBool bVal envVar =
  bool considerEnv' (return True) bVal
  where
    considerEnv' = do
      mEnvVal <- considerEnv envVar
      return $ Just True == mEnvVal

withEnvJwtConf :: Maybe JWTConfig -> String -> WithEnv (Maybe JWTConfig)
withEnvJwtConf jVal envVar =
  maybe (considerEnv envVar) returnJust jVal

mkHGEOptions ::
  L.EnabledLogTypes impl => RawHGEOptions impl -> WithEnv (HGEOptions impl)
mkHGEOptions (HGEOptionsG rawDbUrl rawMetadataDbUrl rawCmd) =
  HGEOptionsG <$> dbUrl <*> metadataDbUrl <*> cmd
  where
    dbUrl = processPostgresConnInfo rawDbUrl
    metadataDbUrl = withEnv rawMetadataDbUrl $ fst metadataDbUrlEnv
    cmd = case rawCmd of
      HCServe rso -> HCServe <$> mkServeOptions rso
      HCExport -> return HCExport
      HCClean -> return HCClean
      HCVersion -> return HCVersion
      HCDowngrade tgt -> return (HCDowngrade tgt)

processPostgresConnInfo ::
  PostgresConnInfo (Maybe PostgresRawConnInfo) ->
  WithEnv (PostgresConnInfo (Maybe UrlConf))
processPostgresConnInfo PostgresConnInfo {..} = do
  withEnvRetries <- withEnv _pciRetries $ fst retriesNumEnv
  databaseUrl <- rawConnInfoToUrlConf _pciDatabaseConn
  pure $ PostgresConnInfo databaseUrl withEnvRetries

rawConnInfoToUrlConf :: Maybe PostgresRawConnInfo -> WithEnv (Maybe UrlConf)
rawConnInfoToUrlConf maybeRawConnInfo = do
  env <- ask
  let databaseUrlEnvVar = fst databaseUrlEnv
      hasDatabaseUrlEnv = any ((== databaseUrlEnvVar) . fst) env

  pure $ case maybeRawConnInfo of
    -- If no --database-url or connection options provided in CLI command
    Nothing ->
      if hasDatabaseUrlEnv
        then -- Consider env variable as is in order to store it as @`UrlConf`
        -- in default source configuration in metadata
          Just $ UrlFromEnv $ T.pack databaseUrlEnvVar
        else Nothing
    Just databaseConn ->
      Just . UrlValue . InputWebhook $ case databaseConn of
        PGConnDatabaseUrl urlTemplate -> urlTemplate
        PGConnDetails connDetails -> rawConnDetailsToUrl connDetails

mkServeOptions :: L.EnabledLogTypes impl => RawServeOptions impl -> WithEnv (ServeOptions impl)
mkServeOptions rso = do
  port <-
    fromMaybe 8080
      <$> withEnv (rsoPort rso) (fst servePortEnv)
  host <-
    fromMaybe "*"
      <$> withEnv (rsoHost rso) (fst serveHostEnv)

  connParams <- mkConnParams $ rsoConnParams rso
  txIso <- fromMaybe Q.ReadCommitted <$> withEnv (rsoTxIso rso) (fst txIsoEnv)
  adminScrt <- fmap (maybe mempty Set.singleton) $ withEnvs (rsoAdminSecret rso) $ map fst [adminSecretEnv, accessKeyEnv]
  authHook <- mkAuthHook $ rsoAuthHook rso
  jwtSecret <- withEnvJwtConf (rsoJwtSecret rso) $ fst jwtSecretEnv
  unAuthRole <- withEnv (rsoUnAuthRole rso) $ fst unAuthRoleEnv
  corsCfg <- mkCorsConfig $ rsoCorsConfig rso
  enableConsole <-
    withEnvBool (rsoEnableConsole rso) $
      fst enableConsoleEnv
  consoleAssetsDir <- withEnv (rsoConsoleAssetsDir rso) (fst consoleAssetsDirEnv)
  enableTelemetry <-
    fromMaybe True
      <$> withEnv (rsoEnableTelemetry rso) (fst enableTelemetryEnv)
  strfyNum <- withEnvBool (rsoStringifyNum rso) $ fst stringifyNumEnv
  dangerousBooleanCollapse <-
    fromMaybe False <$> withEnv (rsoDangerousBooleanCollapse rso) (fst dangerousBooleanCollapseEnv)
  enabledAPIs <-
    Set.fromList . fromMaybe defaultEnabledAPIs
      <$> withEnv (rsoEnabledAPIs rso) (fst enabledAPIsEnv)
  lqOpts <- mkLQOpts
  enableAL <- withEnvBool (rsoEnableAllowlist rso) $ fst enableAllowlistEnv
  enabledLogs <-
    maybe L.defaultEnabledLogTypes Set.fromList
      <$> withEnv (rsoEnabledLogTypes rso) (fst enabledLogsEnv)
  serverLogLevel <- fromMaybe L.LevelInfo <$> withEnv (rsoLogLevel rso) (fst logLevelEnv)
  devMode <- withEnvBool (rsoDevMode rso) $ fst devModeEnv
  adminInternalErrors <-
    fromMaybe True
      <$> withEnv (rsoAdminInternalErrors rso) (fst adminInternalErrorsEnv) -- Default to `true` to enable backwards compatibility
  let internalErrorsConfig =
        if
            | devMode -> InternalErrorsAllRequests
            | adminInternalErrors -> InternalErrorsAdminOnly
            | otherwise -> InternalErrorsDisabled

  eventsHttpPoolSize <- withEnv (rsoEventsHttpPoolSize rso) (fst eventsHttpPoolSizeEnv)
  eventsFetchInterval <- withEnv (rsoEventsFetchInterval rso) (fst eventsFetchIntervalEnv)
  maybeAsyncActionsFetchInterval <- withEnv (rsoAsyncActionsFetchInterval rso) (fst asyncActionsFetchIntervalEnv)
  logHeadersFromEnv <- withEnvBool (rsoLogHeadersFromEnv rso) (fst logHeadersFromEnvEnv)
  enableRemoteSchemaPerms <-
    bool RemoteSchemaPermsDisabled RemoteSchemaPermsEnabled
      <$> withEnvBool (rsoEnableRemoteSchemaPermissions rso) (fst enableRemoteSchemaPermsEnv)

  webSocketCompressionFromEnv <-
    withEnvBool (rsoWebSocketCompression rso) $
      fst webSocketCompressionEnv

  maybeSchemaPollInterval <- withEnv (rsoSchemaPollInterval rso) (fst schemaPollIntervalEnv)

  let connectionOptions =
        WS.defaultConnectionOptions
          { WS.connectionCompressionOptions =
              if webSocketCompressionFromEnv
                then WS.PermessageDeflateCompression WS.defaultPermessageDeflate
                else WS.NoCompression
          }
      asyncActionsFetchInterval = maybe defaultAsyncActionsFetchInterval msToOptionalInterval maybeAsyncActionsFetchInterval
      schemaPollInterval = maybe defaultSchemaPollInterval msToOptionalInterval maybeSchemaPollInterval
  webSocketKeepAlive <-
    KeepAliveDelay . fromIntegral . fromMaybe 5
      <$> withEnv (rsoWebSocketKeepAlive rso) (fst webSocketKeepAliveEnv)

  experimentalFeatures <- maybe mempty Set.fromList <$> withEnv (rsoExperimentalFeatures rso) (fst experimentalFeaturesEnv)
  inferFunctionPerms <-
    maybe FunctionPermissionsInferred (bool FunctionPermissionsManual FunctionPermissionsInferred)
      <$> withEnv (rsoInferFunctionPermissions rso) (fst inferFunctionPermsEnv)

  maintenanceMode <-
    bool MaintenanceModeDisabled MaintenanceModeEnabled
      <$> withEnvBool (rsoEnableMaintenanceMode rso) (fst maintenanceModeEnv)

  eventsFetchBatchSize <-
    fromMaybe defaultFetchBatchSize
      <$> withEnv (rsoEventsFetchBatchSize rso) (fst eventsFetchBatchSizeEnv)

  gracefulShutdownTime <-
    fromMaybe 60 <$> withEnv (rsoGracefulShutdownTimeout rso) (fst gracefulShutdownEnv)

  webSocketConnectionInitTimeout <-
    WSConnectionInitTimeout . fromIntegral . fromMaybe 3
      <$> withEnv (rsoWebSocketConnectionInitTimeout rso) (fst webSocketConnectionInitTimeoutEnv)

  optimizePermissionFilters <-
    fromMaybe False <$> withEnv (rsoOptimizePermissionFilters rso) (fst optimizePermissionFiltersEnv)

  pure $
    ServeOptions
      port
      host
      connParams
      txIso
      adminScrt
      authHook
      jwtSecret
      unAuthRole
      corsCfg
      enableConsole
      consoleAssetsDir
      enableTelemetry
      strfyNum
      dangerousBooleanCollapse
      enabledAPIs
      lqOpts
      enableAL
      enabledLogs
      serverLogLevel
      internalErrorsConfig
      eventsHttpPoolSize
      eventsFetchInterval
      asyncActionsFetchInterval
      logHeadersFromEnv
      enableRemoteSchemaPerms
      connectionOptions
      webSocketKeepAlive
      inferFunctionPerms
      maintenanceMode
      schemaPollInterval
      experimentalFeatures
      eventsFetchBatchSize
      devMode
      gracefulShutdownTime
      webSocketConnectionInitTimeout
      EventingEnabled
      ReadOnlyModeDisabled
      optimizePermissionFilters
  where
    defaultAsyncActionsFetchInterval = Interval 1000 -- 1000 Milliseconds or 1 Second
    defaultSchemaPollInterval = Interval 1000 -- 1000 Milliseconds or 1 Second
    mkConnParams (RawConnParams s c i cl p pt) = do
      stripes <- fromMaybe 1 <$> withEnv s (fst pgStripesEnv)
      -- Note: by Little's Law we can expect e.g. (with 50 max connections) a
      -- hard throughput cap at 1000RPS when db queries take 50ms on average:
      conns <- fromMaybe 50 <$> withEnv c (fst pgConnsEnv)
      iTime <- fromMaybe 180 <$> withEnv i (fst pgTimeoutEnv)
      connLifetime <- withEnv cl (fst pgConnLifetimeEnv) <&> parseConnLifeTime
      allowPrepare <- fromMaybe True <$> withEnv p (fst pgUsePrepareEnv)
      poolTimeout <- withEnv pt (fst pgPoolTimeoutEnv)
      let allowCancel = True
      return $
        Q.ConnParams
          stripes
          conns
          iTime
          allowPrepare
          connLifetime
          poolTimeout
          allowCancel

    mkAuthHook (AuthHookG mUrl mType) = do
      mUrlEnv <- withEnv mUrl $ fst authHookEnv
      authModeM <- withEnv mType (fst authHookModeEnv)
      ty <- onNothing authModeM (authHookTyEnv mType)
      return (flip AuthHookG ty <$> mUrlEnv)

    -- Also support HASURA_GRAPHQL_AUTH_HOOK_TYPE
    -- TODO (from master):- drop this in next major update
    authHookTyEnv mType =
      fromMaybe AHTGet
        <$> withEnv mType "HASURA_GRAPHQL_AUTH_HOOK_TYPE"

    mkCorsConfig mCfg = do
      corsDisabled <- withEnvBool False (fst corsDisableEnv)
      corsCfg <-
        if corsDisabled
          then return (CCDisabled True)
          else fromMaybe CCAllowAll <$> withEnv mCfg (fst corsDomainEnv)

      readCookVal <- withEnvBool (rsoWsReadCookie rso) (fst wsReadCookieEnv)
      wsReadCookie <- case (isCorsDisabled corsCfg, readCookVal) of
        (True, _) -> return readCookVal
        (False, True) ->
          throwError $
            fst wsReadCookieEnv
              <> " can only be used when CORS is disabled"
        (False, False) -> return False
      return $ case corsCfg of
        CCDisabled _ -> CCDisabled wsReadCookie
        _ -> corsCfg

    mkLQOpts = do
      mxRefetchIntM <- withEnv (rsoMxRefetchInt rso) $ fst mxRefetchDelayEnv
      mxBatchSizeM <- withEnv (rsoMxBatchSize rso) $ fst mxBatchSizeEnv
      return $ LQ.mkLiveQueriesOptions mxBatchSizeM mxRefetchIntM

mkExamplesDoc :: [[String]] -> PP.Doc
mkExamplesDoc exampleLines =
  PP.text "Examples: " PP.<$> PP.indent 2 (PP.vsep examples)
  where
    examples = map PP.text $ intercalate [""] exampleLines

mkEnvVarDoc :: [(String, String)] -> PP.Doc
mkEnvVarDoc envVars =
  PP.text "Environment variables: "
    PP.<$> PP.indent 2 (PP.vsep $ map mkEnvVarLine envVars)
  where
    mkEnvVarLine (var, desc) =
      (PP.fillBreak 40 (PP.text var) PP.<+> prettifyDesc desc) <> PP.hardline
    prettifyDesc = PP.align . PP.fillSep . map PP.text . words

mainCmdFooter :: PP.Doc
mainCmdFooter =
  examplesDoc PP.<$> PP.text "" PP.<$> envVarDoc
  where
    examplesDoc = mkExamplesDoc examples
    examples =
      [ [ "# Serve GraphQL Engine on default port (8080) with console disabled",
          "graphql-engine --database-url <database-url> serve"
        ],
        [ "# For more options, checkout",
          "graphql-engine serve --help"
        ]
      ]

    envVarDoc = mkEnvVarDoc [databaseUrlEnv, retriesNumEnv]

databaseUrlEnv :: (String, String)
databaseUrlEnv =
  ( "HASURA_GRAPHQL_DATABASE_URL",
    "Postgres database URL. Example postgres://foo:bar@example.com:2345/database"
  )

metadataDbUrlEnv :: (String, String)
metadataDbUrlEnv =
  ( "HASURA_GRAPHQL_METADATA_DATABASE_URL",
    "Postgres database URL for Metadata storage. Example postgres://foo:bar@example.com:2345/database"
  )

serveCmdFooter :: PP.Doc
serveCmdFooter =
  examplesDoc PP.<$> PP.text "" PP.<$> envVarDoc
  where
    examplesDoc = mkExamplesDoc examples
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

    envVarDoc = mkEnvVarDoc $ envVars <> eventEnvs
    envVars =
      [ accessKeyEnv,
        adminInternalErrorsEnv,
        adminSecretEnv,
        asyncActionsFetchIntervalEnv,
        authHookEnv,
        authHookModeEnv,
        corsDisableEnv,
        corsDomainEnv,
        dangerousBooleanCollapseEnv,
        databaseUrlEnv,
        devModeEnv,
        enableAllowlistEnv,
        enableConsoleEnv,
        enableTelemetryEnv,
        enabledAPIsEnv,
        enabledLogsEnv,
        jwtSecretEnv,
        logLevelEnv,
        pgConnsEnv,
        pgStripesEnv,
        pgTimeoutEnv,
        pgUsePrepareEnv,
        retriesNumEnv,
        serveHostEnv,
        servePortEnv,
        stringifyNumEnv,
        txIsoEnv,
        unAuthRoleEnv,
        webSocketKeepAliveEnv,
        wsReadCookieEnv
      ]

    eventEnvs = [eventsHttpPoolSizeEnv, eventsFetchIntervalEnv]

eventsHttpPoolSizeEnv :: (String, String)
eventsHttpPoolSizeEnv =
  ( "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE",
    "Max event processing threads (default: 100)"
  )

eventsFetchIntervalEnv :: (String, String)
eventsFetchIntervalEnv =
  ( "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL",
    "Interval in milliseconds to sleep before trying to fetch events again after a fetch returned no events from postgres."
  )

eventsFetchBatchSizeEnv :: (String, String)
eventsFetchBatchSizeEnv =
  ( "HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE",
    "The maximum number of events to be fetched from the events table in a single batch. Default 100"
      ++ "Value \"0\" implies completely disable fetching events from events table. "
  )

asyncActionsFetchIntervalEnv :: (String, String)
asyncActionsFetchIntervalEnv =
  ( "HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL",
    "Interval in milliseconds to sleep before trying to fetch new async actions. "
      ++ "Value \"0\" implies completely disable fetching async actions from storage. "
      ++ "Default 1000 milliseconds"
  )

logHeadersFromEnvEnv :: (String, String)
logHeadersFromEnvEnv =
  ( "HASURA_GRAPHQL_LOG_HEADERS_FROM_ENV",
    "Log headers sent instead of logging referenced environment variables."
  )

retriesNumEnv :: (String, String)
retriesNumEnv =
  ( "HASURA_GRAPHQL_NO_OF_RETRIES",
    "No.of retries if Postgres connection error occurs (default: 1)"
  )

servePortEnv :: (String, String)
servePortEnv =
  ( "HASURA_GRAPHQL_SERVER_PORT",
    "Port on which graphql-engine should be served (default: 8080)"
  )

serveHostEnv :: (String, String)
serveHostEnv =
  ( "HASURA_GRAPHQL_SERVER_HOST",
    "Host on which graphql-engine will listen (default: *)"
  )

pgConnsEnv :: (String, String)
pgConnsEnv =
  ( "HASURA_GRAPHQL_PG_CONNECTIONS",
    "Maximum number of Postgres connections that can be opened per stripe (default: 50). "
      <> "When the maximum is reached we will block until a new connection becomes available, "
      <> "even if there is capacity in other stripes."
  )

pgStripesEnv :: (String, String)
pgStripesEnv =
  ( "HASURA_GRAPHQL_PG_STRIPES",
    "Number of stripes (distinct sub-pools) to maintain with Postgres (default: 1). "
      <> "New connections will be taken from a particular stripe pseudo-randomly."
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

pgPoolTimeoutEnv :: (String, String)
pgPoolTimeoutEnv =
  ( "HASURA_GRAPHQL_PG_POOL_TIMEOUT",
    "How long to wait when acquiring a Postgres connection, in seconds (default: forever)."
  )

pgUsePrepareEnv :: (String, String)
pgUsePrepareEnv =
  ( "HASURA_GRAPHQL_USE_PREPARED_STATEMENTS",
    "Use prepared statements for queries (default: true)"
  )

txIsoEnv :: (String, String)
txIsoEnv =
  ( "HASURA_GRAPHQL_TX_ISOLATION",
    "transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)"
  )

accessKeyEnv :: (String, String)
accessKeyEnv =
  ( "HASURA_GRAPHQL_ACCESS_KEY",
    "Admin secret key, required to access this instance (deprecated: use HASURA_GRAPHQL_ADMIN_SECRET instead)"
  )

adminSecretEnv :: (String, String)
adminSecretEnv =
  ( "HASURA_GRAPHQL_ADMIN_SECRET",
    "Admin Secret key, required to access this instance"
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

jwtSecretEnv :: (String, String)
jwtSecretEnv =
  ( "HASURA_GRAPHQL_JWT_SECRET",
    jwtSecretHelp
  )

unAuthRoleEnv :: (String, String)
unAuthRoleEnv =
  ( "HASURA_GRAPHQL_UNAUTHORIZED_ROLE",
    "Unauthorized role, used when admin-secret is not sent in admin-secret only mode "
      ++ "or \"Authorization\" header is absent in JWT mode"
  )

corsDisableEnv :: (String, String)
corsDisableEnv =
  ( "HASURA_GRAPHQL_DISABLE_CORS",
    "Disable CORS. Do not send any CORS headers on any request"
  )

corsDomainEnv :: (String, String)
corsDomainEnv =
  ( "HASURA_GRAPHQL_CORS_DOMAIN",
    "CSV of list of domains, excluding scheme (http/https) and including  port, "
      ++ "to allow CORS for. Wildcard domains are allowed. See docs for details."
  )

enableConsoleEnv :: (String, String)
enableConsoleEnv =
  ( "HASURA_GRAPHQL_ENABLE_CONSOLE",
    "Enable API Console (default: false)"
  )

enableTelemetryEnv :: (String, String)
enableTelemetryEnv =
  ( "HASURA_GRAPHQL_ENABLE_TELEMETRY",
    -- TODO (from master): better description
    "Enable anonymous telemetry (default: true)"
  )

wsReadCookieEnv :: (String, String)
wsReadCookieEnv =
  ( "HASURA_GRAPHQL_WS_READ_COOKIE",
    "Read cookie on WebSocket initial handshake, even when CORS is disabled."
      ++ " This can be a potential security flaw! Please make sure you know "
      ++ "what you're doing."
      ++ "This configuration is only applicable when CORS is disabled."
  )

stringifyNumEnv :: (String, String)
stringifyNumEnv =
  ( "HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES",
    "Stringify numeric types (default: false)"
  )

dangerousBooleanCollapseEnv :: (String, String)
dangerousBooleanCollapseEnv =
  ( "HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE",
    "Emulate V1's behaviour re. boolean expression, where an explicit 'null'"
      <> " value will be interpreted to mean that the field should be ignored"
      <> " [DEPRECATED, WILL BE REMOVED SOON] (default: false)"
  )

defaultEnabledAPIs :: [API]
#ifdef DeveloperAPIs
defaultEnabledAPIs = [METADATA,GRAPHQL,PGDUMP,CONFIG,DEVELOPER]
#else
defaultEnabledAPIs = [METADATA,GRAPHQL,PGDUMP,CONFIG]
#endif

enabledAPIsEnv :: (String, String)
enabledAPIsEnv =
  ( "HASURA_GRAPHQL_ENABLED_APIS",
    "Comma separated list of enabled APIs. (default: metadata,graphql,pgdump,config)"
  )

experimentalFeaturesEnv :: (String, String)
experimentalFeaturesEnv =
  ( "HASURA_GRAPHQL_EXPERIMENTAL_FEATURES",
    "Comma separated list of experimental features. (all: inherited_roles)"
  )

gracefulShutdownEnv :: (String, String)
gracefulShutdownEnv =
  ( "HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT",
    "Timeout for graceful shutdown before which in-flight scheduled events, "
      <> " cron events and async actions to complete (default: 60 seconds)"
  )

consoleAssetsDirEnv :: (String, String)
consoleAssetsDirEnv =
  ( "HASURA_GRAPHQL_CONSOLE_ASSETS_DIR",
    "A directory from which static assets required for console is served at"
      ++ "'/console/assets' path. Can be set to '/srv/console-assets' on the"
      ++ " default docker image to disable loading assets from CDN."
  )

enabledLogsEnv :: (String, String)
enabledLogsEnv =
  ( "HASURA_GRAPHQL_ENABLED_LOG_TYPES",
    "Comma separated list of enabled log types "
      <> "(default: startup,http-log,webhook-log,websocket-log)"
      <> "(all: startup,http-log,webhook-log,websocket-log,query-log)"
  )

logLevelEnv :: (String, String)
logLevelEnv =
  ( "HASURA_GRAPHQL_LOG_LEVEL",
    "Server log level (default: info) (all: error, warn, info, debug)"
  )

devModeEnv :: (String, String)
devModeEnv =
  ( "HASURA_GRAPHQL_DEV_MODE",
    "Set dev mode for GraphQL requests; include 'internal' key in the errors extensions (if required) of the response"
  )

enableRemoteSchemaPermsEnv :: (String, String)
enableRemoteSchemaPermsEnv =
  ( "HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS",
    "Enables remote schema permissions (default: false)"
  )

inferFunctionPermsEnv :: (String, String)
inferFunctionPermsEnv =
  ( "HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS",
    "Infers function permissions (default: true)"
  )

maintenanceModeEnv :: (String, String)
maintenanceModeEnv =
  ( "HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE",
    "Flag to enable maintenance mode in the graphql-engine"
  )

schemaPollIntervalEnv :: (String, String)
schemaPollIntervalEnv =
  ( "HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL",
    "Interval to poll metadata storage for updates in milliseconds - Default 1000 (1s) - Set to 0 to disable"
  )

adminInternalErrorsEnv :: (String, String)
adminInternalErrorsEnv =
  ( "HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS",
    "Enables including 'internal' information in an error response for requests made by an 'admin' (default: true)"
  )

parsePostgresConnInfo :: Parser (PostgresConnInfo (Maybe PostgresRawConnInfo))
parsePostgresConnInfo = do
  retries' <- retries
  maybeRawConnInfo <-
    (fmap PGConnDatabaseUrl <$> parseDatabaseUrl)
      <|> (fmap PGConnDetails <$> parseRawConnDetails)
  pure $ PostgresConnInfo maybeRawConnInfo retries'
  where
    retries =
      optional $
        option
          auto
          ( long "retries"
              <> metavar "NO OF RETRIES"
              <> help (snd retriesNumEnv)
          )

parseDatabaseUrl :: Parser (Maybe URLTemplate)
parseDatabaseUrl =
  optional $
    option
      (eitherReader (parseURLTemplate . T.pack))
      ( long "database-url"
          <> metavar "<DATABASE-URL>"
          <> help (snd databaseUrlEnv)
      )

parseRawConnDetails :: Parser (Maybe PostgresRawConnDetails)
parseRawConnDetails = do
  host' <- host
  port' <- port
  user' <- user
  password' <- password
  dbName' <- dbName
  options' <- options
  pure $
    PostgresRawConnDetails
      <$> host'
      <*> port'
      <*> user'
      <*> pure password'
      <*> dbName'
      <*> pure options'
  where
    host =
      optional $
        strOption
          ( long "host"
              <> metavar "<HOST>"
              <> help "Postgres server host"
          )

    port =
      optional $
        option
          auto
          ( long "port"
              <> short 'p'
              <> metavar "<PORT>"
              <> help "Postgres server port"
          )

    user =
      optional $
        strOption
          ( long "user"
              <> short 'u'
              <> metavar "<USER>"
              <> help "Database user name"
          )

    password =
      strOption
        ( long "password"
            <> metavar "<PASSWORD>"
            <> value ""
            <> help "Password of the user"
        )

    dbName =
      optional $
        strOption
          ( long "dbname"
              <> short 'd'
              <> metavar "<DBNAME>"
              <> help "Database name to connect to"
          )

    options =
      optional $
        strOption
          ( long "pg-connection-options"
              <> short 'o'
              <> metavar "<DATABASE-OPTIONS>"
              <> help "PostgreSQL options"
          )

parseMetadataDbUrl :: Parser (Maybe String)
parseMetadataDbUrl =
  optional $
    strOption
      ( long "metadata-database-url"
          <> metavar "<METADATA-DATABASE-URL>"
          <> help (snd metadataDbUrlEnv)
      )

parseTxIsolation :: Parser (Maybe Q.TxIsolation)
parseTxIsolation =
  optional $
    option
      (eitherReader readIsoLevel)
      ( long "tx-iso"
          <> short 'i'
          <> metavar "<TXISO>"
          <> help (snd txIsoEnv)
      )

parseConnParams :: Parser RawConnParams
parseConnParams =
  RawConnParams <$> stripes <*> conns <*> idleTimeout <*> connLifetime <*> allowPrepare <*> poolTimeout
  where
    stripes =
      optional $
        option
          auto
          ( long "stripes"
              <> short 's'
              <> metavar "<NO OF STRIPES>"
              <> help (snd pgStripesEnv)
          )

    conns =
      optional $
        option
          auto
          ( long "connections"
              <> short 'c'
              <> metavar "<NO OF CONNS>"
              <> help (snd pgConnsEnv)
          )

    idleTimeout =
      optional $
        option
          auto
          ( long "timeout"
              <> metavar "<SECONDS>"
              <> help (snd pgTimeoutEnv)
          )

    connLifetime =
      fmap (fmap (realToFrac :: Int -> NominalDiffTime)) $
        optional $
          option
            auto
            ( long "conn-lifetime"
                <> metavar "<SECONDS>"
                <> help (snd pgConnLifetimeEnv)
            )

    allowPrepare =
      optional $
        option
          (eitherReader parseStringAsBool)
          ( long "use-prepared-statements"
              <> metavar "<true|false>"
              <> help (snd pgUsePrepareEnv)
          )

    poolTimeout =
      fmap (fmap (realToFrac :: Int -> NominalDiffTime)) $
        optional $
          option
            auto
            ( long "pool-timeout"
                <> metavar "<SECONDS>"
                <> help (snd pgPoolTimeoutEnv)
            )

parseServerPort :: Parser (Maybe Int)
parseServerPort =
  optional $
    option
      auto
      ( long "server-port"
          <> metavar "<PORT>"
          <> help (snd servePortEnv)
      )

parseServerHost :: Parser (Maybe HostPreference)
parseServerHost =
  optional $
    strOption
      ( long "server-host"
          <> metavar "<HOST>"
          <> help "Host on which graphql-engine will listen (default: *)"
      )

parseAccessKey :: Parser (Maybe AdminSecretHash)
parseAccessKey =
  optional $
    hashAdminSecret
      <$> strOption
        ( long "access-key"
            <> metavar "ADMIN SECRET KEY (DEPRECATED: USE --admin-secret)"
            <> help (snd adminSecretEnv)
        )

parseAdminSecret :: Parser (Maybe AdminSecretHash)
parseAdminSecret =
  optional $
    hashAdminSecret
      <$> strOption
        ( long "admin-secret"
            <> metavar "ADMIN SECRET KEY"
            <> help (snd adminSecretEnv)
        )

parseWebHook :: Parser RawAuthHook
parseWebHook =
  AuthHookG <$> url <*> urlType
  where
    url =
      optional $
        strOption
          ( long "auth-hook"
              <> metavar "<WEB HOOK URL>"
              <> help (snd authHookEnv)
          )
    urlType =
      optional $
        option
          (eitherReader readHookType)
          ( long "auth-hook-mode"
              <> metavar "<GET|POST>"
              <> help (snd authHookModeEnv)
          )

parseJwtSecret :: Parser (Maybe JWTConfig)
parseJwtSecret =
  optional $
    option
      (eitherReader readJson)
      ( long "jwt-secret"
          <> metavar "<JSON CONFIG>"
          <> help (snd jwtSecretEnv)
      )

jwtSecretHelp :: String
jwtSecretHelp =
  "The JSON containing type and the JWK used for verifying. e.g: "
    <> "`{\"type\": \"HS256\", \"key\": \"<your-hmac-shared-secret>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`,"
    <> "`{\"type\": \"RS256\", \"key\": \"<your-PEM-RSA-public-key>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`"

parseUnAuthRole :: Parser (Maybe RoleName)
parseUnAuthRole =
  fmap mkRoleName' $
    optional $
      strOption
        ( long "unauthorized-role"
            <> metavar "<ROLE>"
            <> help (snd unAuthRoleEnv)
        )
  where
    mkRoleName' mText = mText >>= mkRoleName

parseCorsConfig :: Parser (Maybe CorsConfig)
parseCorsConfig = mapCC <$> disableCors <*> corsDomain
  where
    corsDomain =
      optional $
        option
          (eitherReader readCorsDomains)
          ( long "cors-domain"
              <> metavar "<DOMAINS>"
              <> help (snd corsDomainEnv)
          )

    disableCors =
      switch
        ( long "disable-cors"
            <> help (snd corsDisableEnv)
        )

    mapCC isDisabled domains =
      bool domains (Just $ CCDisabled False) isDisabled

parseEnableConsole :: Parser Bool
parseEnableConsole =
  switch
    ( long "enable-console"
        <> help (snd enableConsoleEnv)
    )

parseConsoleAssetsDir :: Parser (Maybe Text)
parseConsoleAssetsDir =
  optional $
    option
      (eitherReader fromEnv)
      ( long "console-assets-dir"
          <> help (snd consoleAssetsDirEnv)
      )

parseEnableTelemetry :: Parser (Maybe Bool)
parseEnableTelemetry =
  optional $
    option
      (eitherReader parseStringAsBool)
      ( long "enable-telemetry"
          <> help (snd enableTelemetryEnv)
      )

parseWsReadCookie :: Parser Bool
parseWsReadCookie =
  switch
    ( long "ws-read-cookie"
        <> help (snd wsReadCookieEnv)
    )

parseStringifyNum :: Parser Bool
parseStringifyNum =
  switch
    ( long "stringify-numeric-types"
        <> help (snd stringifyNumEnv)
    )

parseDangerousBooleanCollapse :: Parser (Maybe Bool)
parseDangerousBooleanCollapse =
  optional $
    option
      (eitherReader parseStrAsBool)
      ( long "v1-boolean-null-collapse"
          <> help (snd dangerousBooleanCollapseEnv)
      )

parseEnabledAPIs :: Parser (Maybe [API])
parseEnabledAPIs =
  optional $
    option
      (eitherReader readAPIs)
      ( long "enabled-apis"
          <> help (snd enabledAPIsEnv)
      )

parseExperimentalFeatures :: Parser (Maybe [ExperimentalFeature])
parseExperimentalFeatures =
  optional $
    option
      (eitherReader readExperimentalFeatures)
      ( long "experimental-features"
          <> help (snd experimentalFeaturesEnv)
      )

parseGracefulShutdownTimeout :: Parser (Maybe Seconds)
parseGracefulShutdownTimeout =
  optional $
    option
      (eitherReader readEither)
      ( long "graceful-shutdown-timeout"
          <> metavar "<INTERVAL (seconds)>"
          <> help (snd gracefulShutdownEnv)
      )

parseMxRefetchInt :: Parser (Maybe LQ.RefetchInterval)
parseMxRefetchInt =
  optional $
    option
      (eitherReader fromEnv)
      ( long "live-queries-multiplexed-refetch-interval"
          <> metavar "<INTERVAL(ms)>"
          <> help (snd mxRefetchDelayEnv)
      )

parseMxBatchSize :: Parser (Maybe LQ.BatchSize)
parseMxBatchSize =
  optional $
    option
      (eitherReader fromEnv)
      ( long "live-queries-multiplexed-batch-size"
          <> metavar "BATCH_SIZE"
          <> help (snd mxBatchSizeEnv)
      )

parseEnableAllowlist :: Parser Bool
parseEnableAllowlist =
  switch
    ( long "enable-allowlist"
        <> help (snd enableAllowlistEnv)
    )

parseGraphqlDevMode :: Parser Bool
parseGraphqlDevMode =
  switch
    ( long "dev-mode"
        <> help (snd devModeEnv)
    )

parseGraphqlAdminInternalErrors :: Parser (Maybe Bool)
parseGraphqlAdminInternalErrors =
  optional $
    option
      (eitherReader parseStrAsBool)
      ( long "admin-internal-errors"
          <> help (snd adminInternalErrorsEnv)
      )

parseGraphqlEventsHttpPoolSize :: Parser (Maybe Int)
parseGraphqlEventsHttpPoolSize =
  optional $
    option
      (eitherReader fromEnv)
      ( long "events-http-pool-size"
          <> metavar (fst eventsHttpPoolSizeEnv)
          <> help (snd eventsHttpPoolSizeEnv)
      )

parseGraphqlEventsFetchInterval :: Parser (Maybe Milliseconds)
parseGraphqlEventsFetchInterval =
  optional $
    option
      (eitherReader readEither)
      ( long "events-fetch-interval"
          <> metavar (fst eventsFetchIntervalEnv)
          <> help (snd eventsFetchIntervalEnv)
      )

parseEventsFetchBatchSize :: Parser (Maybe NonNegativeInt)
parseEventsFetchBatchSize =
  optional $
    option
      (eitherReader readNonNegativeInt)
      ( long "events-fetch-batch-size"
          <> metavar (fst eventsFetchBatchSizeEnv)
          <> help (snd eventsFetchBatchSizeEnv)
      )

parseGraphqlAsyncActionsFetchInterval :: Parser (Maybe Milliseconds)
parseGraphqlAsyncActionsFetchInterval =
  optional $
    option
      (eitherReader readEither)
      ( long "async-actions-fetch-interval"
          <> metavar (fst asyncActionsFetchIntervalEnv)
          <> help (snd eventsFetchIntervalEnv)
      )

parseLogHeadersFromEnv :: Parser Bool
parseLogHeadersFromEnv =
  switch
    ( long "log-headers-from-env"
        <> help (snd devModeEnv)
    )

parseEnableRemoteSchemaPerms :: Parser Bool
parseEnableRemoteSchemaPerms =
  switch
    ( long "enable-remote-schema-permissions"
        <> help (snd enableRemoteSchemaPermsEnv)
    )

parseInferFunctionPerms :: Parser (Maybe Bool)
parseInferFunctionPerms =
  optional $
    option
      (eitherReader parseStrAsBool)
      ( long "infer-function-permissions"
          <> help (snd inferFunctionPermsEnv)
      )

parseEnableMaintenanceMode :: Parser Bool
parseEnableMaintenanceMode =
  switch
    ( long "enable-maintenance-mode"
        <> help (snd maintenanceModeEnv)
    )

parseSchemaPollInterval :: Parser (Maybe Milliseconds)
parseSchemaPollInterval =
  optional $
    option
      (eitherReader readEither)
      ( long "schema-sync-poll-interval"
          <> metavar (fst schemaPollIntervalEnv)
          <> help (snd schemaPollIntervalEnv)
      )

mxRefetchDelayEnv :: (String, String)
mxRefetchDelayEnv =
  ( "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL",
    "results will only be sent once in this interval (in milliseconds) for "
      <> "live queries which can be multiplexed. Default: 1000 (1sec)"
  )

mxBatchSizeEnv :: (String, String)
mxBatchSizeEnv =
  ( "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE",
    "multiplexed live queries are split into batches of the specified "
      <> "size. Default 100. "
  )

enableAllowlistEnv :: (String, String)
enableAllowlistEnv =
  ( "HASURA_GRAPHQL_ENABLE_ALLOWLIST",
    "Only accept allowed GraphQL queries"
  )

parsePlanCacheSize :: Parser (Maybe Cache.CacheSize)
parsePlanCacheSize =
  optional $
    option
      (eitherReader Cache.parseCacheSize)
      ( long "query-plan-cache-size"
          <> help
            ( "[DEPRECATED: value ignored.] The maximum number of query plans "
                <> "that can be cached, allowed values: 0-65535, "
                <> "0 disables the cache. Default 4000"
            )
      )

parseEnabledLogs :: L.EnabledLogTypes impl => Parser (Maybe [L.EngineLogType impl])
parseEnabledLogs =
  optional $
    option
      (eitherReader L.parseEnabledLogTypes)
      ( long "enabled-log-types"
          <> help (snd enabledLogsEnv)
      )

parseLogLevel :: Parser (Maybe L.LogLevel)
parseLogLevel =
  optional $
    option
      (eitherReader readLogLevel)
      ( long "log-level"
          <> help (snd logLevelEnv)
      )

censorQueryItem :: Text -> QueryItem -> QueryItem
censorQueryItem sensitive (key, Just _) | key == encodeUtf8 sensitive = (key, Just "...")
censorQueryItem _ qi = qi

censorQuery :: Text -> Query -> Query
censorQuery sensitive = fmap (censorQueryItem sensitive)

updateQuery :: (Query -> Query) -> URI -> URI
updateQuery f uri =
  let queries = parseQuery $ pack $ uriQuery uri
   in uri {uriQuery = unpack (renderQuery True $ f queries)}

censorURI :: Text -> URI -> URI
censorURI sensitive uri = updateQuery (censorQuery sensitive) uri

-- Init logging related
connInfoToLog :: Q.ConnInfo -> StartupLog
connInfoToLog connInfo =
  StartupLog L.LevelInfo "postgres_connection" infoVal
  where
    Q.ConnInfo retries details = connInfo
    infoVal = case details of
      Q.CDDatabaseURI uri -> mkDBUriLog $ T.unpack $ bsToTxt uri
      Q.CDOptions co ->
        J.object
          [ "host" J..= Q.connHost co,
            "port" J..= Q.connPort co,
            "user" J..= Q.connUser co,
            "database" J..= Q.connDatabase co,
            "retries" J..= retries
          ]

    mkDBUriLog uri =
      case show . censorURI "sslpassword" <$> parseURI uri of
        Nothing ->
          J.object
            ["error" J..= ("parsing database url failed" :: String)]
        Just s ->
          J.object
            [ "retries" J..= retries,
              "database_url" J..= s
            ]

serveOptsToLog :: J.ToJSON (L.EngineLogType impl) => ServeOptions impl -> StartupLog
serveOptsToLog so =
  StartupLog L.LevelInfo "server_configuration" infoVal
  where
    infoVal =
      J.object
        [ "port" J..= soPort so,
          "server_host" J..= show (soHost so),
          "transaction_isolation" J..= show (soTxIso so),
          "admin_secret_set" J..= not (Set.null (soAdminSecret so)),
          "auth_hook" J..= (ahUrl <$> soAuthHook so),
          "auth_hook_mode" J..= (show . ahType <$> soAuthHook so),
          "jwt_secret" J..= (J.toJSON <$> soJwtSecret so),
          "unauth_role" J..= soUnAuthRole so,
          "cors_config" J..= soCorsConfig so,
          "enable_console" J..= soEnableConsole so,
          "console_assets_dir" J..= soConsoleAssetsDir so,
          "enable_telemetry" J..= soEnableTelemetry so,
          "use_prepared_statements" J..= (Q.cpAllowPrepare . soConnParams) so,
          "stringify_numeric_types" J..= soStringifyNum so,
          "v1-boolean-null-collapse" J..= soDangerousBooleanCollapse so,
          "enabled_apis" J..= soEnabledAPIs so,
          "live_query_options" J..= soLiveQueryOpts so,
          "enable_allowlist" J..= soEnableAllowlist so,
          "enabled_log_types" J..= soEnabledLogTypes so,
          "log_level" J..= soLogLevel so,
          "remote_schema_permissions" J..= soEnableRemoteSchemaPermissions so,
          "websocket_compression_options" J..= show (WS.connectionCompressionOptions . soConnectionOptions $ so),
          "websocket_keep_alive" J..= show (soWebsocketKeepAlive so),
          "infer_function_permissions" J..= soInferFunctionPermissions so,
          "enable_maintenance_mode" J..= soEnableMaintenanceMode so,
          "experimental_features" J..= soExperimentalFeatures so,
          "events_fetch_batch_size" J..= soEventsFetchBatchSize so,
          "graceful_shutdown_timeout" J..= soGracefulShutdownTimeout so,
          "websocket_connection_init_timeout" J..= show (soWebsocketConnectionInitTimeout so)
        ]

mkGenericStrLog :: L.LogLevel -> Text -> String -> StartupLog
mkGenericStrLog logLevel k msg =
  StartupLog logLevel k $ J.toJSON msg

mkGenericLog :: (J.ToJSON a) => L.LogLevel -> Text -> a -> StartupLog
mkGenericLog logLevel k msg =
  StartupLog logLevel k $ J.toJSON msg

inconsistentMetadataLog :: SchemaCache -> StartupLog
inconsistentMetadataLog sc =
  StartupLog L.LevelWarn "inconsistent_metadata" infoVal
  where
    infoVal = J.object ["objects" J..= scInconsistentObjs sc]

serveOptionsParser :: L.EnabledLogTypes impl => Parser (RawServeOptions impl)
serveOptionsParser =
  RawServeOptions
    <$> parseServerPort
    <*> parseServerHost
    <*> parseConnParams
    <*> parseTxIsolation
    <*> (parseAdminSecret <|> parseAccessKey)
    <*> parseWebHook
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
    <*> parseMxRefetchInt
    <*> parseMxBatchSize
    <*> parseEnableAllowlist
    <*> parseEnabledLogs
    <*> parseLogLevel
    <* parsePlanCacheSize -- parsed (for backwards compatibility reasons) but ignored
    <*> parseGraphqlDevMode
    <*> parseGraphqlAdminInternalErrors
    <*> parseGraphqlEventsHttpPoolSize
    <*> parseGraphqlEventsFetchInterval
    <*> parseGraphqlAsyncActionsFetchInterval
    <*> parseLogHeadersFromEnv
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
    <*> parseOptimizePermissionFilters

-- | This implements the mapping between application versions
-- and catalog schema versions.
downgradeShortcuts :: [(String, String)]
downgradeShortcuts =
  $( do
       let s = $(makeRelativeToProject "src-rsr/catalog_versions.txt" >>= embedStringFile)

           parseVersions = map (parseVersion . words) . lines

           parseVersion [tag, version] = (tag, version)
           parseVersion other = error ("unrecognized tag/catalog mapping " ++ show other)
       TH.lift (parseVersions s)
   )

downgradeOptionsParser :: Parser DowngradeOptions
downgradeOptionsParser =
  DowngradeOptions
    <$> choice
      ( strOption
          ( long "to-catalog-version"
              <> metavar "<VERSION>"
              <> help "The target catalog schema version (e.g. 31)"
          ) :
        map (uncurry shortcut) downgradeShortcuts
      )
    <*> switch
      ( long "dryRun"
          <> help "Don't run any migrations, just print out the SQL."
      )
  where
    shortcut v catalogVersion =
      flag'
        (DataString.fromString catalogVersion)
        ( long ("to-" <> v)
            <> help ("Downgrade to graphql-engine version " <> v <> " (equivalent to --to-catalog-version " <> catalogVersion <> ")")
        )

webSocketCompressionEnv :: (String, String)
webSocketCompressionEnv =
  ( "HASURA_GRAPHQL_CONNECTION_COMPRESSION",
    "Enable WebSocket permessage-deflate compression (default: false)"
  )

parseWebSocketCompression :: Parser Bool
parseWebSocketCompression =
  switch
    ( long "websocket-compression"
        <> help (snd webSocketCompressionEnv)
    )

-- NOTE: this is purely used by Apollo-Subscription-Transport-WS
webSocketKeepAliveEnv :: (String, String)
webSocketKeepAliveEnv =
  ( "HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE",
    "Control websocket keep-alive timeout (default 5 seconds)"
  )

parseWebSocketKeepAlive :: Parser (Maybe Int)
parseWebSocketKeepAlive =
  optional $
    option
      (eitherReader readEither)
      ( long "websocket-keepalive"
          <> help (snd webSocketKeepAliveEnv)
      )

-- NOTE: this is purely used by GraphQL-WS
webSocketConnectionInitTimeoutEnv :: (String, String)
webSocketConnectionInitTimeoutEnv =
  ( "HASURA_GRAPHQL_WEBSOCKET_CONNECTION_INIT_TIMEOUT", -- FIXME?: maybe a better name
    "Control websocket connection_init timeout (default 3 seconds)"
  )

parseWebSocketConnectionInitTimeout :: Parser (Maybe Int)
parseWebSocketConnectionInitTimeout =
  optional $
    option
      (eitherReader readEither)
      ( long "websocket-connection-init-timeout"
          <> help (snd webSocketConnectionInitTimeoutEnv)
      )

optimizePermissionFiltersEnv :: (String, String)
optimizePermissionFiltersEnv =
  ( "HASURA_GRAPHQL_OPTIMIZE_PERMISSION_FILTERS",
    "Use experimental SQL optimization transformations for permission filters"
  )

parseOptimizePermissionFilters :: Parser (Maybe Bool)
parseOptimizePermissionFilters =
  optional $
    option
      (eitherReader parseStrAsBool)
      ( long "optimize-permission-filters"
          <> help (snd optimizePermissionFiltersEnv)
      )
