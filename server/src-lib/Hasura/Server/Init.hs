{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 #-}

-- | Arg and Env Parsing for initialisation of the engine along with
-- corresponding logging and other helper functionality.
--
-- This module is intended as the interface for options parsing and
-- its submodules should not need to be imported directly.
module Hasura.Server.Init
  ( -- TODO(SOLOMON): Reduce API surface area drastically.
    module Hasura.Server.Init,
    module Hasura.Server.Init.Config,
    module Hasura.Server.Init.Env,
    module Hasura.Server.Init.Arg,
    module Hasura.Server.Init.Logging,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.TH qualified as J
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Base.Error qualified as Error
import Hasura.Eventing.EventTrigger qualified as EventTrigger
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as RTC
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init.Arg
import Hasura.Server.Init.Config
import Hasura.Server.Init.Env
import Hasura.Server.Init.Logging
import Hasura.Server.Logging qualified as Logging
import Hasura.Server.Types qualified as Types
import Hasura.Server.Utils qualified as Utils
import Network.WebSockets qualified as WS

--------------------------------------------------------------------------------
-- TODO(SOLOMON): Where does this note belong?

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

--------------------------------------------------------------------------------

-- NOTE(SOLOMON): Does this belong here?

-- | Query the Metadata DB for the catalog version.
getDbId :: Q.TxE Error.QErr Types.MetadataDbId
getDbId =
  Types.MetadataDbId . runIdentity . Q.getRow
    <$> Q.withQE
      PG.defaultTxErrorHandler
      [Q.sql|
    SELECT (hasura_uuid :: text) FROM hdb_catalog.hdb_version
  |]
      ()
      False

getPgVersion :: Q.TxE Error.QErr Types.PGVersion
getPgVersion = Types.PGVersion <$> Q.serverVersion

generateInstanceId :: IO Types.InstanceId
generateInstanceId = Types.InstanceId <$> Utils.generateFingerprint

data StartupTimeInfo = StartupTimeInfo
  { _stiMessage :: !Text,
    _stiTimeTaken :: !Double
  }

-- TODO(SOLOMON): Remove this template splice
$(J.deriveJSON hasuraJSON ''StartupTimeInfo)

--------------------------------------------------------------------------------

-- | Given the 'ServeOptionsRaw' parsed from the arg parser,
-- postprocess the db url and fetch env vars associated with the main
-- command parser, then process the subcommand raw values if
-- necessary.
mkHGEOptions ::
  L.EnabledLogTypes impl => HGEOptionsRaw (ServeOptionsRaw impl) -> WithEnv (HGEOptions (ServeOptions impl))
mkHGEOptions (HGEOptionsRaw rawDbUrl rawMetadataDbUrl rawCmd) = do
  dbUrl <- processPostgresConnInfo rawDbUrl
  metadataDbUrl <- withEnv rawMetadataDbUrl $ fst metadataDbUrlEnv
  cmd <- case rawCmd of
    HCServe rso -> HCServe <$> mkServeOptions rso
    HCExport -> pure HCExport
    HCClean -> pure HCClean
    HCVersion -> pure HCVersion
    HCDowngrade tgt -> pure (HCDowngrade tgt)
  pure $ HGEOptions dbUrl metadataDbUrl cmd

-- | 'PostressConnInfo' is a a tuple of some @a@ with a 'Maybe Int'
-- representing the retries setting. This function thus takes a
-- retries setting and a 'PostgresConnInfoRaw' from the arg parser and
-- merges those results with the contents of their corresponding env
-- vars.
processPostgresConnInfo ::
  PostgresConnInfo (Maybe PostgresConnInfoRaw) ->
  WithEnv (PostgresConnInfo (Maybe RTC.UrlConf))
processPostgresConnInfo PostgresConnInfo {..} = do
  withEnvRetries <- withEnv _pciRetries $ fst retriesNumEnv
  databaseUrl <- rawConnInfoToUrlConf _pciDatabaseConn
  pure $ PostgresConnInfo databaseUrl withEnvRetries

-- | A helper function for 'processPostgresConnInfo' which fetches
-- postgres connection info from the 'WithEnv' and merges it with the
-- arg parser result.
rawConnInfoToUrlConf :: Maybe PostgresConnInfoRaw -> WithEnv (Maybe RTC.UrlConf)
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
          Just $ RTC.UrlFromEnv $ T.pack databaseUrlEnvVar
        else Nothing
    Just databaseConn ->
      Just . RTC.UrlValue . RTC.InputWebhook $ case databaseConn of
        PGConnDatabaseUrl urlTemplate -> urlTemplate
        PGConnDetails connDetails -> rawConnDetailsToUrl connDetails

--------------------------------------------------------------------------------
-- TODO(SOLOMON): Decompose 'mkServeOptions'

-- | Merge the results of the serve subcommmand arg parser with
-- corresponding values from the 'WithEnv' context.
mkServeOptions :: L.EnabledLogTypes impl => ServeOptionsRaw impl -> WithEnv (ServeOptions impl)
mkServeOptions rso = do
  port <- fromMaybe 8080 <$> withEnv (rsoPort rso) (fst servePortEnv)

  host <- fromMaybe "*" <$> withEnv (rsoHost rso) (fst serveHostEnv)

  connParams <- mkConnParams $ rsoConnParams rso

  txIso <- fromMaybe Q.ReadCommitted <$> withEnv (rsoTxIso rso) (fst txIsolationEnv)

  adminScrt <- fmap (maybe mempty Set.singleton) $ withEnvs (rsoAdminSecret rso) $ map fst [adminSecretEnv, accessKeyEnv]

  authHook <- mkAuthHook $ rsoAuthHook rso

  jwtSecret <- (`onNothing` mempty) <$> withEnv (rsoJwtSecret rso) (fst jwtSecretEnv)

  unAuthRole <- withEnv (rsoUnAuthRole rso) $ fst unAuthRoleEnv

  corsCfg <- mkCorsConfig $ rsoCorsConfig rso

  enableConsole <- withEnvBool (rsoEnableConsole rso) $ fst enableConsoleEnv

  consoleAssetsDir <- withEnv (rsoConsoleAssetsDir rso) (fst consoleAssetsDirEnv)

  enableTelemetry <- fromMaybe True <$> withEnv (rsoEnableTelemetry rso) (fst enableTelemetryEnv)

  strfyNum <-
    case rsoStringifyNum rso of
      Options.Don'tStringifyNumbers -> fmap (fromMaybe Options.Don'tStringifyNumbers) $ considerEnv (fst stringifyNumEnv)
      stringifyNums -> pure stringifyNums

  dangerousBooleanCollapse <-
    fromMaybe False <$> withEnv (rsoDangerousBooleanCollapse rso) (fst dangerousBooleanCollapseEnv)

  enabledAPIs <-
    Set.fromList . fromMaybe defaultEnabledAPIs
      <$> withEnv (rsoEnabledAPIs rso) (fst enabledAPIsEnv)

  lqOpts <- mkLQOpts

  streamQOpts <- mkStreamQueryOpts

  enableAL <- withEnvBool (rsoEnableAllowlist rso) $ fst enableAllowlistEnv

  enabledLogs <- maybe L.defaultEnabledLogTypes Set.fromList <$> withEnv (rsoEnabledLogTypes rso) (fst enabledLogsEnv)

  serverLogLevel <- fromMaybe L.LevelInfo <$> withEnv (rsoLogLevel rso) (fst logLevelEnv)

  devMode <- withEnvBool (rsoDevMode rso) $ fst graphqlDevModeEnv

  adminInternalErrors <-
    fromMaybe True
      <$> withEnv (rsoAdminInternalErrors rso) (fst graphqlAdminInternalErrorsEnv) -- Default to `true` to enable backwards compatibility
  let internalErrorsConfig =
        if
            | devMode -> InternalErrorsAllRequests
            | adminInternalErrors -> InternalErrorsAdminOnly
            | otherwise -> InternalErrorsDisabled

  eventsHttpPoolSize <- withEnv (rsoEventsHttpPoolSize rso) (fst graphqlEventsHttpPoolSizeEnv)

  eventsFetchInterval <- withEnv (rsoEventsFetchInterval rso) (fst graphqlEventsFetchIntervalEnv)

  asyncActionsFetchInterval <- fromMaybe defaultAsyncActionsFetchInterval <$> withEnv (rsoAsyncActionsFetchInterval rso) (fst asyncActionsFetchIntervalEnv)

  enableRemoteSchemaPerms <-
    case rsoEnableRemoteSchemaPermissions rso of
      Options.DisableRemoteSchemaPermissions -> fmap (fromMaybe Options.DisableRemoteSchemaPermissions) $ considerEnv (fst enableRemoteSchemaPermsEnv)
      enableRemoteSchemaPermissions -> pure enableRemoteSchemaPermissions

  webSocketCompressionFromEnv <-
    withEnvBool (rsoWebSocketCompression rso) $
      fst webSocketCompressionEnv

  schemaPollInterval <- fromMaybe defaultSchemaPollInterval <$> withEnv (rsoSchemaPollInterval rso) (fst schemaPollIntervalEnv)

  let connectionOptions =
        WS.defaultConnectionOptions
          { WS.connectionCompressionOptions =
              if webSocketCompressionFromEnv
                then WS.PermessageDeflateCompression WS.defaultPermessageDeflate
                else WS.NoCompression
          }
  webSocketKeepAlive <- fromMaybe defaultKeepAliveDelay <$> withEnv (rsoWebSocketKeepAlive rso) (fst webSocketKeepAliveEnv)

  experimentalFeatures <- maybe mempty Set.fromList <$> withEnv (rsoExperimentalFeatures rso) (fst experimentalFeaturesEnv)

  inferFunctionPerms <- fromMaybe Options.InferFunctionPermissions <$> withEnv (rsoInferFunctionPermissions rso) (fst inferFunctionPermsEnv)

  maintenanceMode <- case rsoEnableMaintenanceMode rso of
    Types.MaintenanceModeDisabled -> fmap (fromMaybe Types.MaintenanceModeDisabled) $ considerEnv (fst enableMaintenanceModeEnv)
    maintenanceModeEnabled -> pure maintenanceModeEnabled

  eventsFetchBatchSize <-
    fromMaybe EventTrigger.defaultFetchBatchSize
      <$> withEnv (rsoEventsFetchBatchSize rso) (fst eventsFetchBatchSizeEnv)

  gracefulShutdownTime <-
    fromMaybe 60 <$> withEnv (rsoGracefulShutdownTimeout rso) (fst gracefulShutdownEnv)

  webSocketConnectionInitTimeout <-
    fromMaybe defaultWSConnectionInitTimeout <$> withEnv (rsoWebSocketConnectionInitTimeout rso) (fst webSocketConnectionInitTimeoutEnv)

  enableMetadataQueryLogging <- case rsoEnableMetadataQueryLoggingEnv rso of
    Logging.MetadataQueryLoggingDisabled -> fromMaybe Logging.MetadataQueryLoggingDisabled <$> considerEnv (fst enableMetadataQueryLoggingEnv)
    metadataQueryLoggingEnabled -> pure metadataQueryLoggingEnabled

  globalDefaultNamingCase <- withEnv (rsoDefaultNamingConvention rso) $ fst defaultNamingConventionEnv

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
      streamQOpts
      enableAL
      enabledLogs
      serverLogLevel
      internalErrorsConfig
      eventsHttpPoolSize
      eventsFetchInterval
      asyncActionsFetchInterval
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
      Types.EventingEnabled
      Types.ReadOnlyModeDisabled
      enableMetadataQueryLogging
      globalDefaultNamingCase
  where
    defaultAsyncActionsFetchInterval = Interval 1000 -- 1000 Milliseconds or 1 Second
    defaultSchemaPollInterval = Interval 1000 -- 1000 Milliseconds or 1 Second
    mkConnParams (ConnParamsRaw s c i cl p pt) = do
      stripes <- fromMaybe 1 <$> withEnv s (fst pgStripesEnv)
      -- Note: by Little's Law we can expect e.g. (with 50 max connections) a
      -- hard throughput cap at 1000RPS when db queries take 50ms on average:
      conns <- fromMaybe 50 <$> withEnv c (fst pgConnsEnv)
      iTime <- fromMaybe 180 <$> withEnv i (fst pgTimeoutEnv)
      connLifetime <- withEnv cl (fst pgConnLifetimeEnv) <&> Utils.parseConnLifeTime
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

    mkAuthHook (Auth.AuthHookG mUrl mType) = do
      mUrlEnv <- withEnv mUrl $ fst authHookEnv
      authModeM <- withEnv mType (fst authHookModeEnv)
      ty <- onNothing authModeM (authHookTyEnv mType)
      return (flip Auth.AuthHookG ty <$> mUrlEnv)

    -- Also support HASURA_GRAPHQL_AUTH_HOOK_TYPE
    -- TODO (from master):- drop this in next major update
    authHookTyEnv mType =
      fromMaybe Auth.AHTGet
        <$> withEnv mType "HASURA_GRAPHQL_AUTH_HOOK_TYPE"

    mkCorsConfig mCfg = do
      corsDisabled <- withEnvBool False (fst disableCorsEnv)
      corsCfg <-
        if corsDisabled
          then return (Cors.CCDisabled True)
          else fromMaybe Cors.CCAllowAll <$> withEnv mCfg (fst corsDomainEnv)

      readCookVal <- withEnvBool (rsoWsReadCookie rso) (fst wsReadCookieEnv)
      wsReadCookie <- case (Cors.isCorsDisabled corsCfg, readCookVal) of
        (True, _) -> return readCookVal
        (False, True) ->
          throwError $
            fst wsReadCookieEnv
              <> " can only be used when CORS is disabled"
        (False, False) -> return False
      return $ case corsCfg of
        Cors.CCDisabled _ -> Cors.CCDisabled wsReadCookie
        _ -> corsCfg

    mkLQOpts = do
      refetchInterval <- withEnv (rsoMxRefetchInt rso) $ fst mxRefetchDelayEnv
      batchSize <- withEnv (rsoMxBatchSize rso) $ fst mxBatchSizeEnv
      return $ ES.mkSubscriptionsOptions batchSize refetchInterval

    mkStreamQueryOpts = do
      refetchInterval <- withEnv (rsoStreamingMxRefetchInt rso) $ fst streamingMxRefetchDelayEnv
      batchSize <- withEnv (rsoStreamingMxBatchSize rso) $ fst streamingMxBatchSizeEnv
      return $ ES.mkSubscriptionsOptions batchSize refetchInterval

defaultEnabledAPIs :: [API]
defaultEnabledAPIs = [METADATA, GRAPHQL, PGDUMP, CONFIG]
