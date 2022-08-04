{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -O0 #-}

-- | Arg and Env Parsing for initialisation of the engine along with
-- corresponding logging and other helper functionality.
--
-- This module is intended as the interface for options parsing and
-- its submodules should not need to be imported directly.
module Hasura.Server.Init
  ( -- * Option Fetching and Merging
    mkHGEOptions,
    mkServeOptions,
    processPostgresConnInfo,

    -- * Metadata DB
    getDbId,
    getPgVersion,

    -- * Re-exports
    module Hasura.Server.Init.Config,
    module Hasura.Server.Init.Env,
    module Hasura.Server.Init.Arg,
    module Hasura.Server.Init.Logging,
  )
where

--------------------------------------------------------------------------------

import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Database.PG.Query (TxE)
import Database.PG.Query qualified as Query
import Hasura.Backends.Postgres.Connection qualified as Connection
import Hasura.Base.Error (QErr)
import Hasura.Base.Error qualified as Error
import Hasura.Eventing.EventTrigger qualified as EventTrigger
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging (EnabledLogTypes)
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.Common (UrlConf)
import Hasura.RQL.Types.Common qualified as Common
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init.Arg
import Hasura.Server.Init.Config
import Hasura.Server.Init.Env
import Hasura.Server.Init.Logging
import Hasura.Server.Logging qualified as Server.Logging
import Hasura.Server.Types (MetadataDbId)
import Hasura.Server.Types qualified as Types
import Hasura.Server.Utils qualified as Utils
import Network.WebSockets qualified as WebSockets

--------------------------------------------------------------------------------

-- | Query the Metadata DB for the Metadata DB UUID.
-- TODO: Move into a dedicated Metadata module (ala Pro).
getDbId :: TxE QErr MetadataDbId
getDbId =
  Types.MetadataDbId . runIdentity . Query.getRow
    <$> Query.withQE
      Connection.defaultTxErrorHandler
      [Query.sql|
    SELECT (hasura_uuid :: text) FROM hdb_catalog.hdb_version
  |]
      ()
      False

getPgVersion :: Query.TxE Error.QErr Types.PGVersion
getPgVersion = Types.PGVersion <$> Query.serverVersion

--------------------------------------------------------------------------------

-- | Given the 'ServeOptionsRaw' parsed from the arg parser,
-- postprocess the db url and fetch env vars associated with the main
-- command parser, then process the subcommand raw values if
-- necessary.
mkHGEOptions ::
  EnabledLogTypes impl => HGEOptionsRaw (ServeOptionsRaw impl) -> WithEnv (HGEOptions (ServeOptions impl))
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
  WithEnv (PostgresConnInfo (Maybe UrlConf))
processPostgresConnInfo PostgresConnInfo {..} = do
  withEnvRetries <- withEnv _pciRetries $ fst retriesNumEnv
  databaseUrl <- rawConnInfoToUrlConf _pciDatabaseConn
  pure $ PostgresConnInfo databaseUrl withEnvRetries

-- | A helper function for 'processPostgresConnInfo' which fetches
-- postgres connection info from the 'WithEnv' and merges it with the
-- arg parser result.
rawConnInfoToUrlConf :: Maybe PostgresConnInfoRaw -> WithEnv (Maybe UrlConf)
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
          Just $ Common.UrlFromEnv $ Text.pack databaseUrlEnvVar
        else Nothing
    Just databaseConn ->
      Just . Common.UrlValue . Common.InputWebhook $ case databaseConn of
        PGConnDatabaseUrl urlTemplate -> urlTemplate
        PGConnDetails connDetails -> rawConnDetailsToUrl connDetails

--------------------------------------------------------------------------------
-- TODO(SOLOMON): Decompose 'mkServeOptions'

-- | Merge the results of the serve subcommmand arg parser with
-- corresponding values from the 'WithEnv' context.
mkServeOptions :: EnabledLogTypes impl => ServeOptionsRaw impl -> WithEnv (ServeOptions impl)
mkServeOptions rso = do
  port <- fromMaybe 8080 <$> withEnv (rsoPort rso) (fst servePortEnv)

  host <- fromMaybe "*" <$> withEnv (rsoHost rso) (fst serveHostEnv)

  connParams <- mkConnParams $ rsoConnParams rso

  txIso <- fromMaybe Query.ReadCommitted <$> withEnv (rsoTxIso rso) (fst txIsolationEnv)

  adminScrt <- fmap (maybe mempty HashSet.singleton) $ withEnvs (rsoAdminSecret rso) $ map fst [adminSecretEnv, accessKeyEnv]

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
    HashSet.fromList . fromMaybe defaultEnabledAPIs
      <$> withEnv (rsoEnabledAPIs rso) (fst enabledAPIsEnv)

  lqOpts <- mkLQOpts

  streamQOpts <- mkStreamQueryOpts

  enableAL <- withEnvBool (rsoEnableAllowlist rso) $ fst enableAllowlistEnv

  enabledLogs <- maybe Logging.defaultEnabledLogTypes HashSet.fromList <$> withEnv (rsoEnabledLogTypes rso) (fst enabledLogsEnv)

  serverLogLevel <- fromMaybe Logging.LevelInfo <$> withEnv (rsoLogLevel rso) (fst logLevelEnv)

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
        WebSockets.defaultConnectionOptions
          { WebSockets.connectionCompressionOptions =
              if webSocketCompressionFromEnv
                then WebSockets.PermessageDeflateCompression WebSockets.defaultPermessageDeflate
                else WebSockets.NoCompression
          }
  webSocketKeepAlive <- fromMaybe defaultKeepAliveDelay <$> withEnv (rsoWebSocketKeepAlive rso) (fst webSocketKeepAliveEnv)

  experimentalFeatures <- maybe mempty HashSet.fromList <$> withEnv (rsoExperimentalFeatures rso) (fst experimentalFeaturesEnv)

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
    Server.Logging.MetadataQueryLoggingDisabled -> fromMaybe Server.Logging.MetadataQueryLoggingDisabled <$> considerEnv (fst enableMetadataQueryLoggingEnv)
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
        Query.ConnParams
          stripes
          conns
          iTime
          allowPrepare
          connLifetime
          poolTimeout
          allowCancel

    mkAuthHook (AuthHookRaw mUrl mType) = do
      mUrlEnv <- withEnv mUrl $ fst authHookEnv
      authModeM <- withEnv mType (fst authHookModeEnv)
      ty <- onNothing authModeM (authHookTyEnv mType)
      return (flip Auth.AuthHook ty <$> mUrlEnv)

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
