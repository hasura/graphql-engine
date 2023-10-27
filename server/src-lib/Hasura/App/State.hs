{-# LANGUAGE Arrows #-}

module Hasura.App.State
  ( -- * application state
    RebuildableAppContext (..),
    AppEnv (..),
    AppContext (..),
    Loggers (..),

    -- * env access
    HasAppEnv (..),

    -- * init functions
    buildRebuildableAppContext,
    rebuildRebuildableAppContext,

    -- * subsets
    initSQLGenCtx,
    buildCacheStaticConfig,
    buildCacheDynamicConfig,
  )
where

import Control.Arrow.Extended
import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Environment qualified as E
import Data.HashSet qualified as Set
import Database.PG.Query qualified as PG
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.Eventing.Common (LockedEventsCtx)
import Hasura.Eventing.EventTrigger
import Hasura.GraphQL.Execute.Subscription.Options
import Hasura.GraphQL.Execute.Subscription.State qualified as ES
import Hasura.GraphQL.Schema.Common (SchemaSampledFeatureFlags, sampleFeatureFlags)
import Hasura.Incremental qualified as Inc
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache (MetadataResourceVersion)
import Hasura.Server.Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init
import Hasura.Server.Logging
import Hasura.Server.Metrics
import Hasura.Server.Prometheus
import Hasura.Server.Types
import Hasura.ShutdownLatch
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as HTTP
import Network.Wai.Handler.Warp (HostPreference)
import Network.WebSockets.Connection qualified as WebSockets
import Refined (NonNegative, Refined)

--------------------------------------------------------------------------------
-- application state

{- Note [Hasura Application State]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Hasura Application state represents the entire state of hasura.

Hasura Application State = AppEnv (static) + AppContext (dynamic)

Hasura Application State can be divided into two parts:

  1. Read-Only State (Static State):
  =================================
  The information required to build this state is provided only during the
  initialization of hasura. This information is immutable. If you want update any
  field in this state, you would need to shutdown the current instance and
  re-launch hausura with new information.

  Eg: If you want to run hasura in read-only mode, you would have to mention
      this information when hasura starts up. There is no way to make hasura
      run in read-only mode once it has booted up.

  2. Runtime Configurable State (Dynamic State):
  ==============================================
  The information present in this state can be updated during the runtime. This state
  is mutable and does not require a restart of hasura instance to take effect.

  The fields in the state are usually updated via Metadata API's or Hasura Console.

  Eg: You can change the entries in Allowlist via console and hasura need not restart
      for the changes to take effect.

-}

data RebuildableAppContext impl = RebuildableAppContext
  { lastBuiltAppContext :: AppContext,
    _racInvalidationMap :: InvalidationKeys,
    _racRebuild ::
      Inc.Rule
        (ReaderT (L.Logger L.Hasura, HTTP.Manager) (ExceptT QErr IO))
        (ServeOptions impl, E.Environment, InvalidationKeys, CheckFeatureFlag)
        AppContext
  }

-- | Represents the Read-Only Hasura State, these fields are immutable and the state
-- cannot be changed during runtime.
data AppEnv = AppEnv
  { appEnvPort :: Port,
    appEnvHost :: HostPreference,
    appEnvMetadataDbPool :: PG.PGPool,
    appEnvIntrospectionDbPool :: Maybe PG.PGPool,
    appEnvManager :: HTTP.Manager,
    appEnvLoggers :: Loggers,
    appEnvMetadataVersionRef :: STM.TMVar MetadataResourceVersion,
    appEnvInstanceId :: InstanceId,
    appEnvEnableMaintenanceMode :: MaintenanceMode (),
    appEnvLoggingSettings :: LoggingSettings,
    appEnvEventingMode :: EventingMode,
    appEnvEnableReadOnlyMode :: ReadOnlyMode,
    appEnvServerMetrics :: ServerMetrics,
    appEnvShutdownLatch :: ShutdownLatch,
    appEnvMetaVersionRef :: STM.TMVar MetadataResourceVersion,
    appEnvPrometheusMetrics :: PrometheusMetrics,
    appEnvTraceSamplingPolicy :: Tracing.SamplingPolicy,
    appEnvSubscriptionState :: ES.SubscriptionsState,
    appEnvLockedEventsCtx :: LockedEventsCtx,
    appEnvConnParams :: PG.ConnParams,
    appEnvTxIso :: PG.TxIsolation,
    appEnvConsoleAssetsDir :: Maybe Text,
    appEnvConsoleSentryDsn :: Maybe Text,
    appEnvConnectionOptions :: WebSockets.ConnectionOptions,
    appEnvWebSocketKeepAlive :: KeepAliveDelay,
    appEnvWebSocketConnectionInitTimeout :: WSConnectionInitTimeout,
    appEnvGracefulShutdownTimeout :: Refined NonNegative Seconds,
    -- TODO: Move this to `AppContext`. We are leaving this for now as this cannot be changed directly
    -- by the user on the cloud dashboard and will also require a refactor in HasuraPro/App.hs
    -- as this thread is initialised there before creating the `AppStateRef`. But eventually we need
    -- to do it for the Enterprise version.
    appEnvSchemaPollInterval :: OptionalInterval,
    appEnvCheckFeatureFlag :: CheckFeatureFlag,
    appEnvLicenseKeyCache :: Maybe (CredentialCache AgentLicenseKey),
    appEnvMaxTotalHeaderLength :: Int,
    appEnvTriggersErrorLogLevelStatus :: TriggersErrorLogLevelStatus,
    appEnvAsyncActionsFetchBatchSize :: Int,
    appEnvPersistedQueries :: PersistedQueriesState,
    appEnvPersistedQueriesTtl :: Int
  }

-- | Represents the Dynamic Hasura State, these field are mutable and can be changed
-- during runtime.
data AppContext = AppContext
  { acAuthMode :: AuthMode,
    acSQLGenCtx :: SQLGenCtx,
    acEnabledAPIs :: Set.HashSet API,
    acEnableAllowlist :: AllowListStatus,
    acResponseInternalErrorsConfig :: ResponseInternalErrorsConfig,
    acEnvironment :: E.Environment,
    acRemoteSchemaPermsCtx :: Options.RemoteSchemaPermissions,
    acFunctionPermsCtx :: Options.InferFunctionPermissions,
    acExperimentalFeatures :: Set.HashSet ExperimentalFeature,
    acDefaultNamingConvention :: NamingCase,
    acMetadataDefaults :: MetadataDefaults,
    acLiveQueryOptions :: LiveQueriesOptions,
    acStreamQueryOptions :: StreamQueriesOptions,
    acCorsPolicy :: Cors.CorsPolicy,
    acConsoleStatus :: ConsoleStatus,
    acEnableTelemetry :: TelemetryStatus,
    acEventEngineCtx :: EventEngineCtx,
    acAsyncActionsFetchInterval :: OptionalInterval,
    acApolloFederationStatus :: ApolloFederationStatus,
    acCloseWebsocketsOnMetadataChangeStatus :: CloseWebsocketsOnMetadataChangeStatus,
    acSchemaSampledFeatureFlags :: SchemaSampledFeatureFlags
  }

-- | Collection of the LoggerCtx, the regular Logger and the PGLogger
data Loggers = Loggers
  { _lsLoggerCtx :: L.LoggerCtx L.Hasura,
    _lsLogger :: L.Logger L.Hasura,
    _lsPgLogger :: PG.PGLogger
  }

data InvalidationKeys = InvalidationKeys

--------------------------------------------------------------------------------
-- env access

-- | Provides access to the 'AppEnv'.
--
-- This class is nothing more than an equivalent of @MonadReader AppEnv m@, but
-- it abstracts it, so that application code can be written without explicitly
-- relying on an explicit implementation of the app monad. It allows for the app
-- env to be passed implicitly instead of explictly in all of the app init code.
--
-- This class is not meant to be used across the entirety of the codebase, as
-- using it brings in scope the types of all fields, creating dependencies
-- between unrelated parts of the codebase. It is only meant to be used at the
-- top level; more specific parts of the code should only rely on the relevant
-- subset of the environment, exposed by small, local typeclasses. For instance,
-- at time of writing, this can be used to implement 'HasServerConfigCtx', as a
-- first step towards breaking it down.
class (Monad m) => HasAppEnv m where
  askAppEnv :: m AppEnv

instance (HasAppEnv m) => HasAppEnv (ReaderT r m) where
  askAppEnv = lift askAppEnv

instance (HasAppEnv m) => HasAppEnv (ExceptT e m) where
  askAppEnv = lift askAppEnv

--------------------------------------------------------------------------------
-- init functions

initInvalidationKeys :: InvalidationKeys
initInvalidationKeys = InvalidationKeys

-- | Function to build the 'AppContext' (given the 'ServeOptions') for the first
-- time
buildRebuildableAppContext ::
  ( L.Logger L.Hasura,
    HTTP.Manager
  ) ->
  ServeOptions impl ->
  CheckFeatureFlag ->
  E.Environment ->
  ExceptT QErr IO (RebuildableAppContext impl)
buildRebuildableAppContext readerContext serveOptions checkFeatureFlag env = do
  result <- flip runReaderT readerContext $ Inc.build (buildAppContextRule) (serveOptions, env, initInvalidationKeys, checkFeatureFlag)
  let !appContext = Inc.result result
  let !rebuildableAppContext = RebuildableAppContext appContext initInvalidationKeys (Inc.rebuildRule result)
  pure rebuildableAppContext

-- | Function to rebuild the 'AppContext' from a given 'RebuildableAppContext'
-- and a new 'ServeOptions'
rebuildRebuildableAppContext ::
  (MonadIO m, MonadError QErr m) =>
  (L.Logger L.Hasura, HTTP.Manager) ->
  RebuildableAppContext impl ->
  ServeOptions impl ->
  CheckFeatureFlag ->
  E.Environment ->
  m (RebuildableAppContext impl)
rebuildRebuildableAppContext readerCtx (RebuildableAppContext _ _ rule) serveOptions checkFeatureFlag env = do
  let newInvalidationKeys = InvalidationKeys
  result <-
    liftEitherM
      $ liftIO
      $ runExceptT
      $ flip runReaderT readerCtx
      $ Inc.build rule (serveOptions, env, newInvalidationKeys, checkFeatureFlag)
  let appContext = Inc.result result
      !newCtx = RebuildableAppContext appContext newInvalidationKeys (Inc.rebuildRule result)
  pure newCtx

buildAppContextRule ::
  forall arr m impl.
  ( ArrowChoice arr,
    Inc.ArrowCache m arr,
    MonadBaseControl IO m,
    MonadIO m,
    MonadError QErr m,
    MonadReader (L.Logger L.Hasura, HTTP.Manager) m
  ) =>
  (ServeOptions impl, E.Environment, InvalidationKeys, CheckFeatureFlag) `arr` AppContext
buildAppContextRule = proc (ServeOptions {..}, env, _keys, checkFeatureFlag) -> do
  schemaSampledFeatureFlags <- arrM (liftIO . sampleFeatureFlags) -< checkFeatureFlag
  authMode <- buildAuthMode -< (soAdminSecret, soAuthHook, soJwtSecret, soUnAuthRole)
  let sqlGenCtx = initSQLGenCtx soExperimentalFeatures soStringifyNum soDangerousBooleanCollapse soRemoteNullForwardingPolicy
  responseInternalErrorsConfig <- buildResponseInternalErrorsConfig -< (soAdminInternalErrors, soDevMode)
  eventEngineCtx <- buildEventEngineCtx -< (soEventsHttpPoolSize, soEventsFetchInterval, soEventsFetchBatchSize)
  returnA
    -<
      AppContext
        { acAuthMode = authMode,
          acSQLGenCtx = sqlGenCtx,
          acEnabledAPIs = soEnabledAPIs,
          acEnableAllowlist = soEnableAllowList,
          acResponseInternalErrorsConfig = responseInternalErrorsConfig,
          acEnvironment = env,
          acRemoteSchemaPermsCtx = soEnableRemoteSchemaPermissions,
          acFunctionPermsCtx = soInferFunctionPermissions,
          acExperimentalFeatures = soExperimentalFeatures,
          acDefaultNamingConvention = soDefaultNamingConvention,
          acMetadataDefaults = soMetadataDefaults,
          acLiveQueryOptions = soLiveQueryOpts,
          acStreamQueryOptions = soStreamingQueryOpts,
          acCorsPolicy = Cors.mkDefaultCorsPolicy soCorsConfig,
          acConsoleStatus = soConsoleStatus,
          acEnableTelemetry = soEnableTelemetry,
          acEventEngineCtx = eventEngineCtx,
          acAsyncActionsFetchInterval = soAsyncActionsFetchInterval,
          acApolloFederationStatus = soApolloFederationStatus,
          acCloseWebsocketsOnMetadataChangeStatus = soCloseWebsocketsOnMetadataChangeStatus,
          acSchemaSampledFeatureFlags = schemaSampledFeatureFlags
        }
  where
    buildEventEngineCtx = Inc.cache proc (httpPoolSize, fetchInterval, fetchBatchSize) -> do
      eventEngineCtx <- bindA -< initEventEngineCtx httpPoolSize fetchInterval fetchBatchSize
      returnA -< eventEngineCtx

    buildAuthMode :: (Set.HashSet AdminSecretHash, Maybe AuthHook, [JWTConfig], Maybe RoleName) `arr` AuthMode
    buildAuthMode = Inc.cache proc (adminSecretHashSet, webHook, jwtSecrets, unAuthRole) -> do
      authMode <-
        bindA
          -< do
            (logger, httpManager) <- ask
            authModeRes <-
              runExceptT
                $ setupAuthMode
                  adminSecretHashSet
                  webHook
                  jwtSecrets
                  unAuthRole
                  logger
                  httpManager
            onLeft authModeRes throw500
      returnA -< authMode

    buildResponseInternalErrorsConfig :: (AdminInternalErrorsStatus, DevModeStatus) `arr` ResponseInternalErrorsConfig
    buildResponseInternalErrorsConfig = Inc.cache proc (adminInternalErrors, devMode) -> do
      let responseInternalErrorsConfig =
            if
              | isDevModeEnabled devMode -> InternalErrorsAllRequests
              | isAdminInternalErrorsEnabled adminInternalErrors -> InternalErrorsAdminOnly
              | otherwise -> InternalErrorsDisabled
      returnA -< responseInternalErrorsConfig

--------------------------------------------------------------------------------
-- subsets

initSQLGenCtx :: HashSet ExperimentalFeature -> Options.StringifyNumbers -> Options.DangerouslyCollapseBooleans -> Options.RemoteNullForwardingPolicy -> SQLGenCtx
initSQLGenCtx experimentalFeatures stringifyNum dangerousBooleanCollapse remoteNullForwardingPolicy =
  let optimizePermissionFilters
        | EFOptimizePermissionFilters `elem` experimentalFeatures = Options.OptimizePermissionFilters
        | otherwise = Options.Don'tOptimizePermissionFilters

      bigqueryStringNumericInput
        | EFBigQueryStringNumericInput `elem` experimentalFeatures = Options.EnableBigQueryStringNumericInput
        | otherwise = Options.DisableBigQueryStringNumericInput
   in SQLGenCtx stringifyNum dangerousBooleanCollapse remoteNullForwardingPolicy optimizePermissionFilters bigqueryStringNumericInput

buildCacheStaticConfig :: AppEnv -> CacheStaticConfig
buildCacheStaticConfig AppEnv {..} =
  CacheStaticConfig
    { _cscMaintenanceMode = appEnvEnableMaintenanceMode,
      _cscEventingMode = appEnvEventingMode,
      _cscReadOnlyMode = appEnvEnableReadOnlyMode,
      _cscLogger = _lsLogger appEnvLoggers,
      -- Native Queries are always enabled for Postgres in the OSS edition.
      _cscAreNativeQueriesEnabled = \case
        Postgres Vanilla -> True
        DataConnector -> True
        _ -> False,
      _cscAreStoredProceduresEnabled = False
    }

buildCacheDynamicConfig :: AppContext -> CacheDynamicConfig
buildCacheDynamicConfig AppContext {..} = do
  CacheDynamicConfig
    { _cdcFunctionPermsCtx = acFunctionPermsCtx,
      _cdcRemoteSchemaPermsCtx = acRemoteSchemaPermsCtx,
      _cdcSQLGenCtx = acSQLGenCtx,
      _cdcExperimentalFeatures = acExperimentalFeatures,
      _cdcDefaultNamingConvention = acDefaultNamingConvention,
      _cdcMetadataDefaults = acMetadataDefaults,
      _cdcApolloFederationStatus = acApolloFederationStatus,
      _cdcCloseWebsocketsOnMetadataChangeStatus = acCloseWebsocketsOnMetadataChangeStatus,
      _cdcSchemaSampledFeatureFlags = acSchemaSampledFeatureFlags
    }
