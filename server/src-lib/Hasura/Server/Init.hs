{-# LANGUAGE QuasiQuotes #-}

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
    module Hasura.Server.Init.FeatureFlag,
  )
where

--------------------------------------------------------------------------------

import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Database.PG.Query qualified as Query
import Hasura.Backends.Postgres.Connection qualified as Connection
import Hasura.Base.Error qualified as Error
import Hasura.GraphQL.ApolloFederation (getApolloFederationStatus)
import Hasura.GraphQL.Execute.Subscription.Options qualified as Subscription.Options
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init.Arg
import Hasura.Server.Init.Config
import Hasura.Server.Init.Env
import Hasura.Server.Init.FeatureFlag
import Hasura.Server.Init.Logging
import Hasura.Server.Logging qualified as Server.Logging
import Hasura.Server.Types qualified as Types
import Network.WebSockets qualified as WebSockets
import Refined (unrefine)

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

-- | Query the Metadata DB for the Metadata DB UUID.
-- TODO: Move into a dedicated Metadata module (ala Pro).
getDbId :: Query.TxE Error.QErr Types.MetadataDbId
getDbId =
  Types.MetadataDbId
    . runIdentity
    . Query.getRow
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
  (Logging.EnabledLogTypes impl) => HGEOptionsRaw (ServeOptionsRaw impl) -> WithEnv (HGEOptions (ServeOptions impl))
mkHGEOptions (HGEOptionsRaw rawDbUrl rawMetadataDbUrl rawCmd) = do
  dbUrl <- processPostgresConnInfo rawDbUrl
  metadataDbUrl <- withOption rawMetadataDbUrl metadataDbUrlOption
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
  WithEnv (PostgresConnInfo (Maybe Common.UrlConf))
processPostgresConnInfo PostgresConnInfo {..} = do
  withEnvRetries <- withOption _pciRetries retriesNumOption
  databaseUrl <- rawConnInfoToUrlConf _pciDatabaseConn
  pure $ PostgresConnInfo databaseUrl withEnvRetries

-- | A helper function for 'processPostgresConnInfo' which fetches
-- postgres connection info from the 'WithEnv' and merges it with the
-- arg parser result.
rawConnInfoToUrlConf :: Maybe PostgresConnInfoRaw -> WithEnv (Maybe Common.UrlConf)
rawConnInfoToUrlConf maybeRawConnInfo = do
  env <- ask
  let databaseUrlEnvVar = _envVar databaseUrlOption
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

-- | Merge the results of the serve subcommmand arg parser with
-- corresponding values from the 'WithEnv' context.
mkServeOptions :: forall impl. (Logging.EnabledLogTypes impl) => ServeOptionsRaw impl -> WithEnv (ServeOptions impl)
mkServeOptions sor@ServeOptionsRaw {..} = do
  soPort <- withOptionDefault rsoPort servePortOption
  soHost <- withOptionDefault rsoHost serveHostOption
  soConnParams <- mkConnParams rsoConnParams
  soTxIso <- withOptionDefault rsoTxIso txIsolationOption
  soAdminSecret <- maybe mempty (HashSet.singleton) <$> withOptions rsoAdminSecret [adminSecretOption, accessKeyOption]
  soAuthHook <- mkAuthHook rsoAuthHook
  soJwtSecret <- maybeToList <$> withOption rsoJwtSecret jwtSecretOption
  soUnAuthRole <- withOption rsoUnAuthRole unAuthRoleOption
  soCorsConfig <- mkCorsConfig sor rsoCorsConfig
  soConsoleStatus <- withOptionSwitch' rsoConsoleStatus (isConsoleEnabled, bool ConsoleDisabled ConsoleEnabled) enableConsoleOption
  soConsoleAssetsDir <- withOption rsoConsoleAssetsDir consoleAssetsDirOption
  soConsoleSentryDsn <- withOption rsoConsoleSentryDsn consoleSentryDsnOption
  soEnableTelemetry <- withOptionDefault rsoEnableTelemetry enableTelemetryOption
  soStringifyNum <-
    case rsoStringifyNum of
      Options.Don'tStringifyNumbers -> withOptionDefault Nothing stringifyNumOption
      stringifyNums -> pure stringifyNums
  soDangerousBooleanCollapse <- withOptionDefault rsoDangerousBooleanCollapse dangerousBooleanCollapseOption
  soRemoteNullForwardingPolicy <- withOptionDefault rsoRemoteNullForwardingPolicy remoteNullForwardingPolicyOption
  soEnabledAPIs <- withOptionDefault rsoEnabledAPIs enabledAPIsOption
  soLiveQueryOpts <- do
    _lqoRefetchInterval <- withOptionDefault rsoMxRefetchInt mxRefetchDelayOption
    _lqoBatchSize <- withOptionDefault rsoMxBatchSize mxBatchSizeOption
    pure $ Subscription.Options.SubscriptionsOptions {..}
  soStreamingQueryOpts <- do
    _lqoRefetchInterval <- withOptionDefault rsoStreamingMxRefetchInt streamingMxRefetchDelayOption
    _lqoBatchSize <- withOptionDefault rsoStreamingMxBatchSize streamingMxBatchSizeOption
    pure $ Subscription.Options.SubscriptionsOptions {..}
  soEnableAllowList <- withOptionSwitch' rsoEnableAllowList (isAllowListEnabled, bool AllowListDisabled AllowListEnabled) enableAllowlistOption
  soEnabledLogTypes <- withOptionDefault rsoEnabledLogTypes (enabledLogsOption @impl)
  soLogLevel <- withOptionDefault rsoLogLevel logLevelOption
  soDevMode <- withOptionSwitch' rsoDevMode (isDevModeEnabled, bool DevModeDisabled DevModeEnabled) graphqlDevModeOption
  soAdminInternalErrors <- withOptionDefault rsoAdminInternalErrors graphqlAdminInternalErrorsOption
  soEventsHttpPoolSize <- withOptionDefault rsoEventsHttpPoolSize graphqlEventsHttpPoolSizeOption
  soEventsFetchInterval <- withOptionDefault rsoEventsFetchInterval graphqlEventsFetchIntervalOption
  soAsyncActionsFetchInterval <- withOptionDefault rsoAsyncActionsFetchInterval asyncActionsFetchIntervalOption
  soEnableRemoteSchemaPermissions <-
    case rsoEnableRemoteSchemaPermissions of
      Options.DisableRemoteSchemaPermissions -> withOptionDefault Nothing enableRemoteSchemaPermsOption
      enableRemoteSchemaPermissions -> pure enableRemoteSchemaPermissions
  webSocketCompressionFromEnv <-
    withOptionSwitch' rsoWebSocketCompression (isWebSocketCompressionEnabled, bool WebSockets.NoCompression (WebSockets.PermessageDeflateCompression WebSockets.defaultPermessageDeflate)) webSocketCompressionOption
  let soConnectionOptions = WebSockets.defaultConnectionOptions {WebSockets.connectionCompressionOptions = webSocketCompressionFromEnv}
  soWebSocketKeepAlive <- withOptionDefault rsoWebSocketKeepAlive webSocketKeepAliveOption
  soInferFunctionPermissions <- withOptionDefault rsoInferFunctionPermissions inferFunctionPermsOption
  soEnableMaintenanceMode <- case rsoEnableMaintenanceMode of
    Types.MaintenanceModeDisabled -> withOptionDefault Nothing enableMaintenanceModeOption
    maintenanceModeEnabled -> pure maintenanceModeEnabled
  soSchemaPollInterval <- withOptionDefault rsoSchemaPollInterval schemaPollIntervalOption
  soExperimentalFeatures <- withOptionDefault rsoExperimentalFeatures experimentalFeaturesOption
  soEventsFetchBatchSize <- withOptionDefault rsoEventsFetchBatchSize eventsFetchBatchSizeOption
  soGracefulShutdownTimeout <- withOptionDefault rsoGracefulShutdownTimeout gracefulShutdownOption
  soWebSocketConnectionInitTimeout <- withOptionDefault rsoWebSocketConnectionInitTimeout webSocketConnectionInitTimeoutOption
  let soEventingMode = Types.EventingEnabled
  let soReadOnlyMode = Types.ReadOnlyModeDisabled
  soEnableMetadataQueryLogging <- case rsoEnableMetadataQueryLoggingEnv of
    Server.Logging.MetadataQueryLoggingDisabled -> withOptionDefault Nothing enableMetadataQueryLoggingOption
    metadataQueryLoggingEnabled -> pure metadataQueryLoggingEnabled
  soDefaultNamingConvention <- withOptionDefault rsoDefaultNamingConvention defaultNamingConventionOption
  soMetadataDefaults <- withOptionDefault rsoMetadataDefaults metadataDefaultsOption
  soExtensionsSchema <- withOptionDefault rsoExtensionsSchema metadataDBExtensionsSchemaOption
  soApolloFederationStatus <- do
    apolloFederationStatusOptionM <- withOptionDefault (pure <$> rsoApolloFederationStatus) apolloFederationStatusOption
    pure $ getApolloFederationStatus soExperimentalFeatures apolloFederationStatusOptionM
  soCloseWebsocketsOnMetadataChangeStatus <- do
    withOptionDefault rsoCloseWebsocketsOnMetadataChangeStatus closeWebsocketsOnMetadataChangeOption
  soMaxTotalHeaderLength <- withOptionDefault rsoMaxTotalHeaderLength maxTotalHeaderLengthOption
  soTriggersErrorLogLevelStatus <- withOptionDefault rsoTriggersErrorLogLevelStatus triggersErrorLogLevelStatusOption
  soAsyncActionsFetchBatchSize <- withOptionDefault rsoAsyncActionsFetchBatchSize asyncActionsFetchBatchSizeOption
  soPersistedQueries <- withOptionDefault rsoPersistedQueries persistedQueriesOption
  soPersistedQueriesTtl <- withOptionDefault rsoPersistedQueriesTtl persistedQueriesTtlOption
  pure ServeOptions {..}

-- | Fetch Postgres 'Query.ConnParams' components from the environment
-- and merge with the values consumed by the arg parser in
-- 'ConnParamsRaw'.
mkConnParams :: (Monad m) => ConnParamsRaw -> WithEnvT m Query.ConnParams
mkConnParams ConnParamsRaw {..} = do
  cpStripes <- unrefine <$> withOptionDefault rcpStripes pgStripesOption
  -- Note: by Little's Law we can expect e.g. (with 50 max connections) a
  -- hard throughput cap at 1000RPS when db queries take 50ms on average:
  cpConns <- unrefine <$> withOptionDefault rcpConns pgConnsOption
  cpIdleTime <- unrefine <$> withOptionDefault rcpIdleTime pgTimeoutOption
  cpAllowPrepare <- withOptionDefault rcpAllowPrepare pgUsePreparedStatementsOption
  -- TODO: Add newtype to allow this:
  cpMbLifetime <- do
    lifetime <- unrefine <$> withOptionDefault rcpConnLifetime pgConnLifetimeOption
    if lifetime == 0
      then pure Nothing
      else pure (Just lifetime)
  cpTimeout <- fmap unrefine <$> withOption rcpPoolTimeout pgPoolTimeoutOption
  let cpCancel = True
  return
    $ Query.ConnParams {..}

-- | Fetch 'Auth.AuthHook' components from the environment and merge
-- with the values consumed by the arg parser in 'AuthHookRaw'.
mkAuthHook :: (Monad m) => AuthHookRaw -> WithEnvT m (Maybe Auth.AuthHook)
mkAuthHook (AuthHookRaw mUrl mType mSendRequestBody) = do
  mUrlEnv <- withOption mUrl authHookOption
  -- Also support HASURA_GRAPHQL_AUTH_HOOK_TYPE
  -- TODO (from master):- drop this in next major update <--- (NOTE: This comment is from 2020-08-21)
  authMode <-
    onNothing
      mType
      ( fromMaybe (_default authHookModeOption)
          <$> considerEnvs
            [_envVar authHookModeOption, "HASURA_GRAPHQL_AUTH_HOOK_TYPE"]
      )
  -- if authMode is `GET` then authSendRequestBody is set to `False`, otherwise we check for the config value
  authSendRequestBody <-
    case authMode of
      Auth.AHTGet -> pure False
      Auth.AHTPost ->
        onNothing
          mSendRequestBody
          ( fromMaybe (_default authHookSendRequestBodyOption)
              <$> considerEnvs
                [_envVar authHookSendRequestBodyOption]
          )
  pure $ (\url -> Auth.AuthHook url authMode authSendRequestBody) <$> mUrlEnv

-- | Fetch 'Cors.CorsConfig' settings from the environment and merge
-- with the settings consumed by the arg parser.
mkCorsConfig :: (Monad m) => ServeOptionsRaw imp -> Maybe Cors.CorsConfig -> WithEnvT m Cors.CorsConfig
mkCorsConfig ServeOptionsRaw {..} mCfg = do
  corsCfg <- do
    corsDisabled <- withOptionDefault Nothing disableCorsOption
    if corsDisabled
      then pure (Cors.CCDisabled $ _default disableCorsOption)
      else withOptionDefault mCfg corsDomainOption
  readCookVal <-
    case rsoWsReadCookie of
      WsReadCookieDisabled -> withOptionDefault Nothing wsReadCookieOption
      p -> pure p
  wsReadCookie <- case (Cors.isCorsDisabled corsCfg, readCookVal) of
    (True, _) -> pure readCookVal
    (False, WsReadCookieEnabled) ->
      throwError
        $ _envVar wsReadCookieOption
        <> " can only be used when CORS is disabled"
    (False, WsReadCookieDisabled) -> pure WsReadCookieDisabled
  pure $ case corsCfg of
    Cors.CCDisabled _ -> Cors.CCDisabled $ isWsReadCookieEnabled wsReadCookie
    _ -> corsCfg
