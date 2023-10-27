{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Defines the CE version of the engine.
--
-- This module contains everything that is required to run the community edition
-- of the engine: the base application monad and the implementation of all its
-- behaviour classes.
module Hasura.App
  ( -- * top-level error handling
    ExitCode (..),
    ExitException (..),
    throwErrExit,
    throwErrJExit,
    accessDeniedErrMsg,

    -- * printing helpers
    printJSON,

    -- * logging
    mkLoggers,
    mkPGLogger,

    -- * basic connection info
    BasicConnectionInfo (..),
    initMetadataConnectionInfo,
    initBasicConnectionInfo,
    resolvePostgresConnInfo,

    -- * app init
    initialiseAppEnv,
    initialiseAppContext,
    migrateCatalogAndFetchMetadata,
    buildFirstSchemaCache,
    initSubscriptionsState,
    initLockedEventsCtx,

    -- * app monad
    AppM,
    runAppM,

    -- * misc
    getCatalogStateTx,
    updateJwkCtxThread,
    notifySchemaCacheSyncTx,
    parseArgs,
    runHGEServer,
    setCatalogStateTx,
    mkHGEServer,
    mkPgSourceResolver,
    mkMSSQLSourceResolver,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.Extended (sleep)
import Control.Concurrent.Extended qualified as C
import Control.Concurrent.STM
import Control.Concurrent.STM qualified as STM
import Control.Exception (bracket_, throwIO)
import Control.Monad.Catch
  ( Exception,
    MonadCatch,
    MonadMask,
    MonadThrow,
  )
import Control.Monad.Morph (hoist)
import Control.Monad.Stateless
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Managed (ManagedT (..), allocate, allocate_)
import Control.Retry qualified as Retry
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Environment qualified as Env
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict qualified as HashMap
import Data.Set.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import Database.MSSQL.Pool qualified as MSPool
import Database.PG.Query qualified as PG
import Database.PG.Query qualified as Q
import GHC.AssertNF.CPP
import Hasura.App.State
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.ClientCredentials (getEEClientCredentialsTx, setEEClientCredentialsTx)
import Hasura.Eventing.Backend
import Hasura.Eventing.Common
import Hasura.Eventing.EventTrigger
import Hasura.Eventing.ScheduledTrigger
import Hasura.GraphQL.Execute
  ( ExecutionStep (..),
    MonadGQLExecutionCheck (..),
    checkQueryInAllowlist,
  )
import Hasura.GraphQL.Execute.Action
import Hasura.GraphQL.Execute.Action.Subscription
import Hasura.GraphQL.Execute.Subscription.Poll qualified as ES
import Hasura.GraphQL.Execute.Subscription.State qualified as ES
import Hasura.GraphQL.Logging (MonadExecutionLog (..), MonadQueryLog (..))
import Hasura.GraphQL.Transport.HTTP
  ( CacheResult (..),
    MonadExecuteQuery (..),
  )
import Hasura.GraphQL.Transport.HTTP.Protocol (toParsed)
import Hasura.GraphQL.Transport.WSServerApp qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Types (WSServerEnv (..))
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.PingSources
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup (..))
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.DDL.Schema.Catalog
import Hasura.RQL.DDL.SchemaRegistry qualified as SchemaRegistry
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.ResizePool
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.API.Query (requiresAdmin)
import Hasura.Server.App
import Hasura.Server.AppStateRef
import Hasura.Server.Auth
import Hasura.Server.CheckUpdates (checkForUpdates)
import Hasura.Server.Init
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Migrate (migrateCatalog)
import Hasura.Server.Prometheus
  ( PrometheusMetrics (..),
    decWarpThreads,
    incWarpThreads,
  )
import Hasura.Server.ResourceChecker (getServerResources)
import Hasura.Server.SchemaUpdate
import Hasura.Server.Telemetry
import Hasura.Server.Types
import Hasura.Server.Version
import Hasura.Services
import Hasura.Session
import Hasura.ShutdownLatch
import Hasura.Tracing
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.CreateManager (mkHttpManager)
import Network.Types.Extended
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import Refined (unrefine)
import System.Metrics qualified as EKG
import System.Metrics.Gauge qualified as EKG.Gauge
import Text.Mustache.Compile qualified as M
import Web.Spock.Core qualified as Spock

--------------------------------------------------------------------------------
-- Error handling (move to another module!)

data ExitCode
  = -- these are used during server initialization:
    InvalidEnvironmentVariableOptionsError
  | InvalidDatabaseConnectionParamsError
  | AuthConfigurationError
  | DatabaseMigrationError
  | -- | used by MT because it initialises the schema cache only
    -- these are used in app/Main.hs:
    SchemaCacheInitError
  | MetadataExportError
  | MetadataCleanError
  | DowngradeProcessError
  deriving (Show)

data ExitException = ExitException
  { eeCode :: !ExitCode,
    eeMessage :: !BC.ByteString
  }
  deriving (Show)

instance Exception ExitException

throwErrExit :: (MonadIO m) => forall a. ExitCode -> String -> m a
throwErrExit reason = liftIO . throwIO . ExitException reason . BC.pack

throwErrJExit :: (J.ToJSON a, MonadIO m) => forall b. ExitCode -> a -> m b
throwErrJExit reason = liftIO . throwIO . ExitException reason . BLC.toStrict . J.encode

accessDeniedErrMsg :: Text
accessDeniedErrMsg = "restricted access : admin only"

--------------------------------------------------------------------------------
-- Printing helpers (move to another module!)

printJSON :: (J.ToJSON a, MonadIO m) => a -> m ()
printJSON = liftIO . BLC.putStrLn . J.encode

--------------------------------------------------------------------------------
-- Logging

mkPGLogger :: Logger Hasura -> PG.PGLogger
mkPGLogger (Logger logger) (PG.PLERetryMsg msg) = logger $ PGLog LevelWarn msg

-- | Create all loggers based on the set of enabled logs and chosen log level.
mkLoggers ::
  (MonadIO m, MonadBaseControl IO m) =>
  HashSet (EngineLogType Hasura) ->
  LogLevel ->
  ManagedT m Loggers
mkLoggers enabledLogs logLevel = do
  loggerCtx <- mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
  let logger = mkLogger loggerCtx
      pgLogger = mkPGLogger logger
  pure $ Loggers loggerCtx logger pgLogger

--------------------------------------------------------------------------------
-- Basic connection info

-- | Basic information required to connect to the metadata DB, and to the
-- default Postgres DB if any.
data BasicConnectionInfo = BasicConnectionInfo
  { -- | metadata db connection info
    bciMetadataConnInfo :: PG.ConnInfo,
    -- | default postgres connection info, if any
    bciDefaultPostgres :: Maybe PostgresConnConfiguration
  }

-- | Only create the metadata connection info.
--
-- Like 'initBasicConnectionInfo', it prioritizes @--metadata-database-url@, and
-- falls back to @--database-url@ otherwise.
--
-- !!! This function throws a fatal error if the @--database-url@ cannot be
-- !!! resolved.
initMetadataConnectionInfo ::
  (MonadIO m) =>
  Env.Environment ->
  -- | metadata DB URL (--metadata-database-url)
  Maybe String ->
  -- | user's DB URL (--database-url)
  PostgresConnInfo (Maybe UrlConf) ->
  m PG.ConnInfo
initMetadataConnectionInfo env metadataDbURL dbURL =
  fmap bciMetadataConnInfo
    $ initBasicConnectionInfo
      env
      metadataDbURL
      dbURL
      Nothing -- ignored
      False -- ignored
      PG.ReadCommitted -- ignored

-- | Create a 'BasicConnectionInfo' based on the given options.
--
-- The default postgres connection is only created when the @--database-url@
-- option is given. If the @--metadata-database-url@ isn't given, the
-- @--database-url@ will be used for the metadata connection.
--
-- All arguments related to the default postgres connection are ignored if the
-- @--database-url@ is missing.
--
-- !!! This function throws a fatal error if the @--database-url@ cannot be
-- !!! resolved.
initBasicConnectionInfo ::
  (MonadIO m) =>
  Env.Environment ->
  -- | metadata DB URL (--metadata-database-url). This is expected to be either
  -- a postgres connection string URI, or our own @dynamic-from-file@ URI,
  -- corresponding to the dynamic_from_file feature when connecting a postgres
  -- data source.
  Maybe String ->
  -- | user's DB URL (--database-url)
  PostgresConnInfo (Maybe UrlConf) ->
  -- | pool settings of the default PG connection
  Maybe PostgresPoolSettings ->
  -- | whether the default PG config should use prepared statements
  Bool ->
  -- | default transaction isolation level
  PG.TxIsolation ->
  m BasicConnectionInfo
initBasicConnectionInfo
  env
  metadataDbUrl
  (PostgresConnInfo dbUrlConf maybeRetries)
  poolSettings
  usePreparedStatements
  isolationLevel =
    case (metadataDbUrl, dbUrlConf) of
      (Nothing, Nothing) ->
        throwErrExit
          InvalidDatabaseConnectionParamsError
          "Fatal Error: Either of --metadata-database-url or --database-url option expected"
      -- If no metadata storage specified consider use default database as
      -- metadata storage
      (Nothing, Just srcURL) -> do
        srcConnInfo <- resolvePostgresConnInfo env srcURL maybeRetries
        pure $ BasicConnectionInfo srcConnInfo (Just $ mkSourceConfig srcURL)
      (Just mdURL, Nothing) ->
        pure $ BasicConnectionInfo (mkConnInfoFromMDB mdURL) Nothing
      (Just mdURL, Just srcURL) -> do
        _srcConnInfo <- resolvePostgresConnInfo env srcURL maybeRetries
        pure $ BasicConnectionInfo (mkConnInfoFromMDB mdURL) (Just $ mkSourceConfig srcURL)
    where
      -- Our own made up URI scheme corresponding to the dynamic_from_file feature
      -- NOTE: Since this can only be set by the administrator, there's no need
      -- to validate against something like HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX
      dynamicScheme = "dynamic-from-file://"
      mkConnInfoFromMDB mdbURL =
        PG.ConnInfo
          { PG.ciRetries = fromMaybe 1 maybeRetries,
            PG.ciDetails = case splitAt (length dynamicScheme) mdbURL of
              (mbDynamicScheme, mbPath)
                | mbDynamicScheme == dynamicScheme ->
                    PG.CDDynamicDatabaseURI mbPath
              _ -> PG.CDDatabaseURI $ txtToBs $ T.pack mdbURL
          }
      mkSourceConfig srcURL =
        PostgresConnConfiguration
          { pccConnectionInfo =
              PostgresSourceConnInfo
                { psciDatabaseUrl = srcURL,
                  psciPoolSettings = poolSettings,
                  psciUsePreparedStatements = usePreparedStatements,
                  psciIsolationLevel = isolationLevel,
                  psciSslConfiguration = Nothing
                },
            pccReadReplicas = Nothing,
            pccExtensionsSchema = defaultPostgresExtensionsSchema,
            pccConnectionTemplate = Nothing,
            pccConnectionSet = mempty
          }

-- | Creates a 'PG.ConnInfo' from a 'UrlConf' parameter.
--
-- !!! throws a fatal error if the configuration is invalid
resolvePostgresConnInfo ::
  (MonadIO m) =>
  Env.Environment ->
  UrlConf ->
  Maybe Int ->
  m PG.ConnInfo
resolvePostgresConnInfo env dbUrlConf (fromMaybe 1 -> retries) = do
  connDetails <-
    runExceptT (resolveUrlConf env dbUrlConf)
      >>= either (liftIO . throwErrJExit InvalidDatabaseConnectionParamsError) pure
  pure $ PG.ConnInfo retries connDetails

--------------------------------------------------------------------------------
-- App init

-- | The initialisation of the app is split into several functions, for clarity;
-- but there are several pieces of information that need to be threaded across
-- those initialisation functions. This small data structure groups together all
-- such pieces of information that are required throughout the initialisation,
-- but that aren't needed in the rest of the application.
data AppInit = AppInit
  { aiTLSAllowListRef :: TLSAllowListRef,
    aiMetadataWithResourceVersion :: MetadataWithResourceVersion
  }

-- | Initializes or migrates the catalog and creates the 'AppEnv' required to
-- start the server, and also create the 'AppInit' that needs to be threaded
-- along the init code.
--
-- For historical reasons, this function performs a few additional startup tasks
-- that are not required to create the 'AppEnv', such as starting background
-- processes and logging startup information. All of those are flagged with a
-- comment marking them as a side-effect.
--
-- NOTE: this is invoked in pro, but only for OSS mode (no license key)
initialiseAppEnv ::
  (C.ForkableMonadIO m) =>
  Env.Environment ->
  BasicConnectionInfo ->
  ServeOptions Hasura ->
  Maybe ES.SubscriptionPostPollHook ->
  ServerMetrics ->
  PrometheusMetrics ->
  SamplingPolicy ->
  ManagedT m (AppInit, AppEnv)
initialiseAppEnv env BasicConnectionInfo {..} serveOptions@ServeOptions {..} liveQueryHook serverMetrics prometheusMetrics traceSamplingPolicy = do
  loggers@(Loggers _loggerCtx logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel

  -- SIDE EFFECT: print a warning if no admin secret is set.
  when (null soAdminSecret)
    $ unLogger
      logger
      StartupLog
        { slLogLevel = LevelWarn,
          slKind = "no_admin_secret",
          slInfo = J.toJSON ("WARNING: No admin secret provided" :: Text)
        }

  -- SIDE EFFECT: log all server options.
  unLogger logger $ serveOptsToLog serveOptions

  -- SIDE EFFECT: log metadata postgres connection info.
  unLogger logger $ connInfoToLog bciMetadataConnInfo

  -- Generate the instance id.
  instanceId <- liftIO generateInstanceId

  let connectionContext :: J.Value
      connectionContext = J.object ["source" J..= J.String "metadata"]

  -- Init metadata db pool.
  metadataDbPool <-
    allocate
      (liftIO $ PG.initPGPool bciMetadataConnInfo connectionContext soConnParams pgLogger)
      (liftIO . PG.destroyPGPool)

  -- Migrate the catalog and fetch the metdata.
  metadataWithVersion <-
    lift
      $ migrateCatalogAndFetchMetadata
        logger
        metadataDbPool
        bciDefaultPostgres
        soEnableMaintenanceMode
        soExtensionsSchema

  -- Create the TLSAllowListRef and the HTTP Manager.
  let metadata = _mwrvMetadata metadataWithVersion
  tlsAllowListRef <- liftIO $ createTLSAllowListRef $ networkTlsAllowlist $ _metaNetwork metadata
  httpManager <- liftIO $ mkHttpManager (readTLSAllowList tlsAllowListRef) mempty

  -- Start a background thread for listening schema sync events from other
  -- server instances (an interval of 0 indicates that no schema sync is
  -- required). Logs whether the thread is started or not, and with what
  -- interval.
  -- TODO: extract into a separate init function.
  metaVersionRef <- liftIO $ STM.newEmptyTMVarIO
  case soSchemaPollInterval of
    Skip -> unLogger logger $ mkGenericLog @Text LevelInfo "schema-sync" "Schema sync disabled"
    Interval interval -> do
      unLogger logger $ mkGenericLog @String LevelInfo "schema-sync" ("Schema sync enabled. Polling at " <> show interval)
      void $ startSchemaSyncListenerThread logger metadataDbPool instanceId interval metaVersionRef

  -- Generate the shutdown latch.
  latch <- liftIO newShutdownLatch

  -- Generate subscription state.
  subscriptionsState <- liftIO $ initSubscriptionsState logger liveQueryHook

  -- Generate event's trigger shared state
  lockedEventsCtx <- liftIO $ initLockedEventsCtx

  pure
    ( AppInit
        { aiTLSAllowListRef = tlsAllowListRef,
          aiMetadataWithResourceVersion = metadataWithVersion
        },
      AppEnv
        { appEnvPort = soPort,
          appEnvHost = soHost,
          appEnvMetadataDbPool = metadataDbPool,
          appEnvIntrospectionDbPool = Nothing, -- No introspection storage for self-hosted CE
          appEnvManager = httpManager,
          appEnvLoggers = loggers,
          appEnvMetadataVersionRef = metaVersionRef,
          appEnvInstanceId = instanceId,
          appEnvEnableMaintenanceMode = soEnableMaintenanceMode,
          appEnvLoggingSettings = LoggingSettings soEnabledLogTypes soEnableMetadataQueryLogging,
          appEnvEventingMode = soEventingMode,
          appEnvEnableReadOnlyMode = soReadOnlyMode,
          appEnvServerMetrics = serverMetrics,
          appEnvShutdownLatch = latch,
          appEnvMetaVersionRef = metaVersionRef,
          appEnvPrometheusMetrics = prometheusMetrics,
          appEnvTraceSamplingPolicy = traceSamplingPolicy,
          appEnvSubscriptionState = subscriptionsState,
          appEnvLockedEventsCtx = lockedEventsCtx,
          appEnvConnParams = soConnParams,
          appEnvTxIso = soTxIso,
          appEnvConsoleAssetsDir = soConsoleAssetsDir,
          appEnvConsoleSentryDsn = soConsoleSentryDsn,
          appEnvConnectionOptions = soConnectionOptions,
          appEnvWebSocketKeepAlive = soWebSocketKeepAlive,
          appEnvWebSocketConnectionInitTimeout = soWebSocketConnectionInitTimeout,
          appEnvGracefulShutdownTimeout = soGracefulShutdownTimeout,
          appEnvCheckFeatureFlag = ceCheckFeatureFlag env,
          appEnvSchemaPollInterval = soSchemaPollInterval,
          appEnvLicenseKeyCache = Nothing,
          appEnvMaxTotalHeaderLength = soMaxTotalHeaderLength,
          appEnvTriggersErrorLogLevelStatus = soTriggersErrorLogLevelStatus,
          appEnvAsyncActionsFetchBatchSize = soAsyncActionsFetchBatchSize,
          appEnvPersistedQueries = soPersistedQueries,
          appEnvPersistedQueriesTtl = soPersistedQueriesTtl
        }
    )

-- | Initializes the 'AppContext' and returns a corresponding 'AppStateRef'.
--
-- This function is meant to be run in the app monad, which provides the
-- 'AppEnv'.
initialiseAppContext ::
  (C.ForkableMonadIO m, HasAppEnv m) =>
  Env.Environment ->
  ServeOptions Hasura ->
  AppInit ->
  m (AppStateRef Hasura)
initialiseAppContext env serveOptions AppInit {..} = do
  appEnv@AppEnv {..} <- askAppEnv
  let cacheStaticConfig = buildCacheStaticConfig appEnv
      Loggers _ logger pgLogger = appEnvLoggers

  -- Build the RebuildableAppContext.
  -- (See note [Hasura Application State].)
  rebuildableAppCtxE <-
    liftIO
      $ runExceptT
        ( buildRebuildableAppContext
            (logger, appEnvManager)
            serveOptions
            appEnvCheckFeatureFlag
            env
        )
  !rebuildableAppCtx <- onLeft rebuildableAppCtxE $ \e -> throwErrExit InvalidEnvironmentVariableOptionsError $ T.unpack $ qeError e

  let cacheDynamicConfig = buildCacheDynamicConfig (lastBuiltAppContext rebuildableAppCtx)

  -- Create the schema cache
  rebuildableSchemaCache <-
    buildFirstSchemaCache
      env
      logger
      (mkPgSourceResolver pgLogger)
      mkMSSQLSourceResolver
      aiMetadataWithResourceVersion
      cacheStaticConfig
      cacheDynamicConfig
      appEnvManager
      Nothing
  -- Initialise the 'AppStateRef' from 'RebuildableSchemaCacheRef' and 'RebuildableAppContext'.
  initialiseAppStateRef aiTLSAllowListRef Nothing appEnvServerMetrics rebuildableSchemaCache rebuildableAppCtx

-- | Runs catalogue migration, and returns the metadata that was fetched.
--
-- On success, this function logs the result of the migration, on failure it
-- logs a 'catalog_migrate' error and throws a fatal error.
migrateCatalogAndFetchMetadata ::
  (MonadIO m, MonadBaseControl IO m) =>
  Logger Hasura ->
  PG.PGPool ->
  Maybe (SourceConnConfiguration ('Postgres 'Vanilla)) ->
  MaintenanceMode () ->
  ExtensionsSchema ->
  m MetadataWithResourceVersion
migrateCatalogAndFetchMetadata
  logger
  pool
  defaultSourceConfig
  maintenanceMode
  extensionsSchema = do
    -- TODO: should we allow the migration to happen during maintenance mode?
    -- Allowing this can be a sanity check, to see if the hdb_catalog in the
    -- DB has been set correctly
    currentTime <- liftIO Clock.getCurrentTime
    result <-
      runExceptT
        $ PG.runTx pool (PG.Serializable, Just PG.ReadWrite)
        $ migrateCatalog
          defaultSourceConfig
          extensionsSchema
          maintenanceMode
          currentTime
    case result of
      Left err -> do
        unLogger
          logger
          StartupLog
            { slLogLevel = LevelError,
              slKind = "catalog_migrate",
              slInfo = J.toJSON err
            }
        liftIO (throwErrJExit DatabaseMigrationError err)
      Right (migrationResult, metadataWithVersion) -> do
        unLogger logger migrationResult
        pure metadataWithVersion

-- | Build the original 'RebuildableSchemaCache'.
--
-- On error, it logs a 'catalog_migrate' error and throws a fatal error. This
-- misnomer is intentional: it is to preserve a previous behaviour of the code
-- and avoid a breaking change.
buildFirstSchemaCache ::
  (MonadIO m) =>
  Env.Environment ->
  Logger Hasura ->
  SourceResolver ('Postgres 'Vanilla) ->
  SourceResolver ('MSSQL) ->
  MetadataWithResourceVersion ->
  CacheStaticConfig ->
  CacheDynamicConfig ->
  HTTP.Manager ->
  Maybe SchemaRegistry.SchemaRegistryContext ->
  m RebuildableSchemaCache
buildFirstSchemaCache
  env
  logger
  pgSourceResolver
  mssqlSourceResolver
  metadataWithVersion
  cacheStaticConfig
  cacheDynamicConfig
  httpManager
  mSchemaRegistryContext = do
    let cacheBuildParams = CacheBuildParams httpManager pgSourceResolver mssqlSourceResolver cacheStaticConfig
    result <-
      runExceptT
        $ runCacheBuild cacheBuildParams
        $ buildRebuildableSchemaCache logger env metadataWithVersion cacheDynamicConfig mSchemaRegistryContext
    result `onLeft` \err -> do
      -- TODO: we used to bundle the first schema cache build with the catalog
      -- migration, using the same error handler for both, meaning that an
      -- error in the first schema cache build would be reported as
      -- follows. Changing this will be a breaking change.
      unLogger
        logger
        StartupLog
          { slLogLevel = LevelError,
            slKind = "catalog_migrate",
            slInfo = J.toJSON err
          }
      liftIO (throwErrJExit DatabaseMigrationError err)

initSubscriptionsState ::
  Logger Hasura ->
  Maybe ES.SubscriptionPostPollHook ->
  IO ES.SubscriptionsState
initSubscriptionsState logger liveQueryHook = ES.initSubscriptionsState postPollHook
  where
    postPollHook = fromMaybe (ES.defaultSubscriptionPostPollHook logger) liveQueryHook

initLockedEventsCtx :: IO LockedEventsCtx
initLockedEventsCtx =
  liftM4
    LockedEventsCtx
    (STM.newTVarIO mempty)
    (STM.newTVarIO mempty)
    (STM.newTVarIO mempty)
    (STM.newTVarIO mempty)

--------------------------------------------------------------------------------
-- App monad

-- | The base app monad of the CE engine.
newtype AppM a = AppM (ReaderT AppEnv (TraceT IO) a)
  deriving newtype
    ( Functor,
      MonadFail, -- only due to https://gitlab.haskell.org/ghc/ghc/-/issues/15681
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadCatch,
      MonadThrow,
      MonadMask,
      MonadReader AppEnv,
      MonadBase IO,
      MonadBaseControl IO
    )

runAppM :: AppEnv -> AppM a -> IO a
runAppM c (AppM a) = ignoreTraceT $ runReaderT a c

instance HasAppEnv AppM where
  askAppEnv = ask

instance HasFeatureFlagChecker AppM where
  checkFlag f = AppM do
    CheckFeatureFlag {runCheckFeatureFlag} <- asks appEnvCheckFeatureFlag
    liftIO $ runCheckFeatureFlag f

instance HasCacheStaticConfig AppM where
  askCacheStaticConfig = buildCacheStaticConfig <$> askAppEnv

instance MonadTrace AppM where
  newTraceWith c p n (AppM a) = AppM $ newTraceWith c p n a
  newSpanWith i n (AppM a) = AppM $ newSpanWith i n a
  attachMetadata = AppM . attachMetadata

instance MonadTraceContext AppM where
  currentContext = AppM currentContext

instance ProvidesNetwork AppM where
  askHTTPManager = asks appEnvManager

instance HasResourceLimits AppM where
  askHTTPHandlerLimit = pure $ ResourceLimits id
  askGraphqlOperationLimit _ _ _ = pure $ ResourceLimits id

instance HttpLog AppM where
  type ExtraHttpLogMetadata AppM = ()

  emptyExtraHttpLogMetadata = ()

  buildExtraHttpLogMetadata _ _ = ()

  logHttpError logger loggingSettings userInfoM reqId waiReq req qErr headers _ _ =
    unLoggerTracing logger
      $ mkHttpLog
      $ mkHttpErrorLogContext userInfoM loggingSettings reqId waiReq req qErr Nothing Nothing headers

  logHttpSuccess logger loggingSettings userInfoM reqId waiReq reqBody response compressedResponse qTime cType headers (CommonHttpLogMetadata rb batchQueryOpLogs, ()) _ =
    unLoggerTracing logger
      $ mkHttpLog
      $ mkHttpAccessLogContext userInfoM loggingSettings reqId waiReq reqBody (BL.length response) compressedResponse qTime cType headers rb batchQueryOpLogs

instance MonadExecuteQuery AppM where
  cacheLookup _ _ _ _ _ _ = pure $ Right ([], ResponseUncached Nothing)

instance UserAuthentication AppM where
  resolveUserInfo logger manager headers authMode reqs =
    runExceptT $ do
      (a, b, c) <- getUserInfoWithExpTime logger manager headers authMode reqs
      pure $ (a, b, c, ExtraUserInfo Nothing)

instance MonadMetadataApiAuthorization AppM where
  authorizeV1QueryApi query handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (requiresAdmin query && currRole /= adminRoleName)
      $ withPathK "args"
      $ throw400 AccessDenied accessDeniedErrMsg

  authorizeV1MetadataApi _ handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (currRole /= adminRoleName)
      $ withPathK "args"
      $ throw400 AccessDenied accessDeniedErrMsg

  authorizeV2QueryApi _ handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (currRole /= adminRoleName)
      $ withPathK "args"
      $ throw400 AccessDenied accessDeniedErrMsg

instance ConsoleRenderer AppM where
  type ConsoleType AppM = CEConsoleType
  renderConsole path authMode enableTelemetry consoleAssetsDir consoleSentryDsn consoleType =
    return $ mkConsoleHTML path authMode enableTelemetry consoleAssetsDir consoleSentryDsn consoleType

instance MonadVersionAPIWithExtraData AppM where
  -- we always default to CE as the `server_type` in this codebase
  getExtraDataForVersionAPI = return ["server_type" J..= ("ce" :: Text)]

instance MonadGQLExecutionCheck AppM where
  checkGQLExecution userInfo _ enableAL sc query _ = runExceptT $ do
    req <- toParsed query
    checkQueryInAllowlist enableAL AllowlistModeGlobalOnly userInfo req sc
    return req

  executeIntrospection _ introspectionQuery _ =
    pure $ Right $ ExecStepRaw introspectionQuery

  checkGQLBatchedReqs _ _ _ _ = runExceptT $ pure ()

instance MonadConfigApiHandler AppM where
  runConfigApiHandler = configApiGetHandler

instance MonadGQLApiHandler AppM where
  runPersistedQueriesGetHandler _ _ _ = throw400 NotSupported "PersistedQueryNotSupported"
  runPersistedQueriesPostHandler _ _ query =
    case query of
      EqrGQLReq gqlReq -> v1GQHandler gqlReq
      EqrAPQReq _ -> throw400 NotSupported "PersistedQueryNotSupported"

instance MonadQueryLog AppM where
  logQueryLog logger = unLoggerTracing logger

instance MonadExecutionLog AppM where
  logExecutionLog logger = unLoggerTracing logger

instance WS.MonadWSLog AppM where
  logWSLog logger = unLoggerTracing logger

instance MonadResolveSource AppM where
  getPGSourceResolver = asks (mkPgSourceResolver . _lsPgLogger . appEnvLoggers)
  getMSSQLSourceResolver = return mkMSSQLSourceResolver

instance MonadQueryTags AppM where
  createQueryTags _attributes _qtSourceConfig = return $ emptyQueryTagsComment

instance MonadEventLogCleanup AppM where
  runLogCleaner _ _ = pure $ throw400 NotSupported "Event log cleanup feature is enterprise edition only"
  generateCleanupSchedules _ _ _ = pure $ Right ()
  updateTriggerCleanupSchedules _ _ _ _ = pure $ Right ()

instance MonadGetPolicies AppM where
  runGetApiTimeLimit = pure $ Nothing
  runGetPrometheusMetricsGranularity = pure (pure GranularMetricsOff)
  runGetModelInfoLogStatus = pure (pure ModelInfoLogOff)

-- | Each of the function in the type class is executed in a totally separate transaction.
instance MonadMetadataStorage AppM where
  fetchMetadataResourceVersion = runInSeparateTx fetchMetadataResourceVersionFromCatalog
  fetchMetadata = runInSeparateTx fetchMetadataAndResourceVersionFromCatalog
  fetchMetadataNotifications a b = runInSeparateTx $ fetchMetadataNotificationsFromCatalog a b
  setMetadata r = runInSeparateTx . setMetadataInCatalog r
  notifySchemaCacheSync a b c = runInSeparateTx $ notifySchemaCacheSyncTx a b c
  getCatalogState = runInSeparateTx getCatalogStateTx
  setCatalogState a b = runInSeparateTx $ setCatalogStateTx a b

  -- stored source introspection is not available in this distribution
  fetchSourceIntrospection _ = pure $ Right Nothing
  storeSourceIntrospection _ _ = pure $ Right ()

  getMetadataDbUid = runInSeparateTx getDbId
  checkMetadataStorageHealth = runInSeparateTx $ checkDbConnection

  getDeprivedCronTriggerStats = runInSeparateTx . getDeprivedCronTriggerStatsTx
  getScheduledEventsForDelivery = runInSeparateTx . getScheduledEventsForDeliveryTx
  insertCronEvents = runInSeparateTx . insertCronEventsTx
  insertOneOffScheduledEvent = runInSeparateTx . insertOneOffScheduledEventTx
  insertScheduledEventInvocation a b = runInSeparateTx $ insertInvocationTx a b
  setScheduledEventOp a b c = runInSeparateTx $ setScheduledEventOpTx a b c
  unlockScheduledEvents a b = runInSeparateTx $ unlockScheduledEventsTx a b
  unlockAllLockedScheduledEvents = runInSeparateTx unlockAllLockedScheduledEventsTx
  clearFutureCronEvents = runInSeparateTx . dropFutureCronEventsTx
  getOneOffScheduledEvents a b c = runInSeparateTx $ getOneOffScheduledEventsTx a b c
  getCronEvents a b c d = runInSeparateTx $ getCronEventsTx a b c d
  getScheduledEventInvocations a = runInSeparateTx $ getScheduledEventInvocationsTx a
  deleteScheduledEvent a b = runInSeparateTx $ deleteScheduledEventTx a b

  insertAction a b c d = runInSeparateTx $ insertActionTx a b c d
  fetchUndeliveredActionEvents a = runInSeparateTx $ fetchUndeliveredActionEventsTx a
  setActionStatus a b = runInSeparateTx $ setActionStatusTx a b
  fetchActionResponse = runInSeparateTx . fetchActionResponseTx
  clearActionData = runInSeparateTx . clearActionDataTx
  setProcessingActionLogsToPending = runInSeparateTx . setProcessingActionLogsToPendingTx

instance MonadEECredentialsStorage AppM where
  getEEClientCredentials = runInSeparateTx getEEClientCredentialsTx
  setEEClientCredentials a = runInSeparateTx $ setEEClientCredentialsTx a

--------------------------------------------------------------------------------
-- misc

-- TODO(SOLOMON): Move Into `Hasura.Server.Init`. Unable to do so
-- currently due `throwErrExit`.

-- | Parse cli arguments to graphql-engine executable.
parseArgs :: (EnabledLogTypes impl) => Env.Environment -> IO (HGEOptions (ServeOptions impl))
parseArgs env = do
  rawHGEOpts <- execParser opts
  let eitherOpts = runWithEnv (Env.toList env) $ mkHGEOptions rawHGEOpts
  onLeft eitherOpts $ throwErrExit InvalidEnvironmentVariableOptionsError
  where
    opts =
      info
        (helper <*> parseHgeOpts)
        ( fullDesc
            <> header "Hasura GraphQL Engine: Blazing fast, instant realtime GraphQL APIs on your DB with fine grained access control, also trigger webhooks on database events."
            <> footerDoc (Just mainCmdFooter)
        )

-- | Core logic to fork a poller thread to update the JWK based on the
-- expiry time specified in @Expires@ header or @Cache-Control@ header
updateJwkCtxThread ::
  (C.ForkableMonadIO m) =>
  IO AppContext ->
  HTTP.Manager ->
  Logger Hasura ->
  m Void
updateJwkCtxThread getAppCtx httpManager logger = forever $ do
  authMode <- liftIO $ acAuthMode <$> getAppCtx
  updateJwkCtx authMode httpManager logger
  liftIO $ sleep $ seconds 1

-- | Event triggers live in the user's DB and other events
--  (cron, one-off and async actions)
--   live in the metadata DB, so we need a way to differentiate the
--   type of shutdown action
data ShutdownAction
  = EventTriggerShutdownAction (IO ())
  | MetadataDBShutdownAction (ExceptT QErr IO ())

-- | This function acts as the entrypoint for the graphql-engine webserver.
--
-- Note: at the exit of this function, or in case of a graceful server shutdown
-- (SIGTERM, or more generally, whenever the shutdown latch is set),  we need to
-- make absolutely sure that we clean up any resources which were allocated during
-- server setup. In the case of a multitenant process, failure to do so can lead to
-- resource leaks.
--
-- To track these resources, we use the ManagedT monad, and attach finalizers at
-- the same point in the code where we allocate resources. If you fork a new
-- long-lived thread, or create a connection pool, or allocate any other
-- long-lived resource, make sure to pair the allocator  with its finalizer.
-- There are plenty of examples throughout the code. For example, see
-- 'C.forkManagedT'.
--
-- Note also: the order in which the finalizers run can be important. Specifically,
-- we want the finalizers for the logger threads to run last, so that we retain as
-- many "thread stopping" log messages as possible. The order in which the
-- finalizers is run is determined by the order in which they are introduced in the
-- code.

{- HLINT ignore runHGEServer "Avoid lambda" -}
{- HLINT ignore runHGEServer "Use withAsync" -}
runHGEServer ::
  forall m impl.
  ( MonadIO m,
    MonadFail m, -- only due to https://gitlab.haskell.org/ghc/ghc/-/issues/15681
    MonadFix m,
    MonadMask m,
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    UserAuthentication m,
    HttpLog m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    MonadMetadataApiAuthorization m,
    MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    MonadExecutionLog m,
    WS.MonadWSLog m,
    MonadExecuteQuery m,
    HasResourceLimits m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesHasuraServices m,
    MonadTrace m,
    MonadGetPolicies m,
    MonadGQLApiHandler m
  ) =>
  (AppStateRef impl -> Spock.SpockT m ()) ->
  AppStateRef impl ->
  -- | start time
  UTCTime ->
  -- | A hook which can be called to indicate when the server is started succesfully
  Maybe (IO ()) ->
  ConsoleType m ->
  EKG.Store EKG.EmptyMetrics ->
  ManagedT m ()
runHGEServer setupHook appStateRef initTime startupStatusHook consoleType ekgStore = do
  AppEnv {..} <- lift askAppEnv
  waiApplication <- mkHGEServer setupHook appStateRef consoleType ekgStore

  let logger = _lsLogger appEnvLoggers
  -- `startupStatusHook`: add `Service started successfully` message to config_status
  -- table when a tenant starts up in multitenant
  let warpSettings :: Warp.Settings
      warpSettings =
        Warp.setPort (_getPort appEnvPort)
          . Warp.setHost appEnvHost
          . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
          . Warp.setInstallShutdownHandler shutdownHandler
          . Warp.setBeforeMainLoop (for_ startupStatusHook id)
          . Warp.setServerName ""
          . setForkIOWithMetrics
          . Warp.setMaxTotalHeaderLength appEnvMaxTotalHeaderLength
          $ Warp.defaultSettings

      setForkIOWithMetrics :: Warp.Settings -> Warp.Settings
      setForkIOWithMetrics = Warp.setFork \f -> do
        void
          $ C.forkIOWithUnmask
            ( \unmask ->
                bracket_
                  ( do
                      EKG.Gauge.inc (smWarpThreads appEnvServerMetrics)
                      incWarpThreads (pmConnections appEnvPrometheusMetrics)
                  )
                  ( do
                      EKG.Gauge.dec (smWarpThreads appEnvServerMetrics)
                      decWarpThreads (pmConnections appEnvPrometheusMetrics)
                  )
                  (f unmask)
            )

      shutdownHandler :: IO () -> IO ()
      shutdownHandler closeSocket =
        LA.link =<< LA.async do
          waitForShutdown appEnvShutdownLatch
          unLogger logger $ mkGenericLog @Text LevelInfo "server" "gracefully shutting down server"
          closeSocket

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  lift
    $ unLoggerTracing logger
    $ mkGenericLog LevelInfo "server"
    $ StartupTimeInfo "starting API server" apiInitTime

  -- Here we block until the shutdown latch 'MVar' is filled, and then
  -- shut down the server. Once this blocking call returns, we'll tidy up
  -- any resources using the finalizers attached using 'ManagedT' above.
  -- Structuring things using the shutdown latch in this way lets us decide
  -- elsewhere exactly how we want to control shutdown.
  liftIO $ Warp.runSettings warpSettings waiApplication

-- | Part of a factorization of 'runHGEServer' to expose the constructed WAI
-- application for testing purposes. See 'runHGEServer' for documentation.
mkHGEServer ::
  forall m impl.
  ( MonadIO m,
    MonadFail m, -- only due to https://gitlab.haskell.org/ghc/ghc/-/issues/15681
    MonadFix m,
    MonadMask m,
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    UserAuthentication m,
    HttpLog m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    MonadMetadataApiAuthorization m,
    MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    MonadExecutionLog m,
    WS.MonadWSLog m,
    MonadExecuteQuery m,
    HasResourceLimits m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesHasuraServices m,
    MonadTrace m,
    MonadGetPolicies m,
    MonadGQLApiHandler m
  ) =>
  (AppStateRef impl -> Spock.SpockT m ()) ->
  AppStateRef impl ->
  ConsoleType m ->
  EKG.Store EKG.EmptyMetrics ->
  ManagedT m Application
mkHGEServer setupHook appStateRef consoleType ekgStore = do
  -- Comment this to enable expensive assertions from "GHC.AssertNF". These
  -- will log lines to STDOUT containing "not in normal form". In the future we
  -- could try to integrate this into our tests. For now this is a development
  -- tool.
  --
  -- NOTE: be sure to compile WITHOUT code coverage, for this to work properly.
  liftIO disableAssertNF
  AppEnv {..} <- lift askAppEnv
  let Loggers loggerCtx logger _ = appEnvLoggers

  wsServerEnv <- lift $ WS.createWSServerEnv appStateRef

  HasuraApp app actionSubState stopWsServer <-
    lift
      $ mkWaiApp
        setupHook
        appStateRef
        consoleType
        ekgStore
        wsServerEnv

  -- Log Warning if deprecated environment variables are used
  sources <- scSources <$> liftIO (getSchemaCache appStateRef)
  -- TODO: naveen: send IO to logDeprecatedEnvVars
  AppContext {..} <- liftIO $ getAppContext appStateRef
  liftIO $ logDeprecatedEnvVars logger acEnvironment sources

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSchemaCache appStateRef)
  liftIO $ logInconsistentMetadata logger inconsObjs

  -- NOTE: `newLogTVar` is being used to make sure that the metadata logger runs only once
  --       while logging errors or any `inconsistent_metadata` logs.
  newLogTVar <- liftIO $ STM.newTVarIO False

  -- Start a background thread for processing schema sync event present in the '_sscSyncEventRef'
  _ <- startSchemaSyncProcessorThread appStateRef newLogTVar

  case appEnvEventingMode of
    EventingEnabled -> do
      startEventTriggerPollerThread logger appEnvLockedEventsCtx
      startAsyncActionsPollerThread logger appEnvLockedEventsCtx actionSubState

      -- Create logger for logging the statistics of fetched cron triggers
      fetchedCronTriggerStatsLogger <-
        allocate
          (createFetchedCronTriggerStatsLogger logger)
          (closeFetchedCronTriggersStatsLogger logger)

      -- start a background thread to create new cron events
      _cronEventsThread <-
        C.forkManagedT "runCronEventsGenerator" logger
          $ runCronEventsGenerator logger fetchedCronTriggerStatsLogger (getSchemaCache appStateRef)

      startScheduledEventsPollerThread logger appEnvLockedEventsCtx
    EventingDisabled ->
      lift $ unLoggerTracing logger $ mkGenericLog @Text LevelInfo "server" "starting in eventing disabled mode"

  -- start a background thread to check for updates
  _updateThread <-
    C.forkManagedT "checkForUpdates" logger
      $ liftIO
      $ checkForUpdates loggerCtx appEnvManager

  -- Start a background thread for source pings
  _sourcePingPoller <-
    C.forkManagedT "sourcePingPoller" logger $ do
      let pingLog =
            unLogger logger . mkGenericLog @String LevelInfo "sources-ping"
      liftIO
        ( runPingSources
            acEnvironment
            pingLog
            (scSourcePingConfig <$> getSchemaCache appStateRef)
        )

  -- initialise the websocket connection reaper thread
  _websocketConnectionReaperThread <-
    C.forkManagedT "websocket connection reaper thread" logger
      $ liftIO
      $ WS.websocketConnectionReaper getLatestConfigForWSServer getSchemaCache' (_wseServer wsServerEnv)

  dbUid <-
    getMetadataDbUid `onLeftM` throwErrJExit DatabaseMigrationError
  pgVersion <-
    liftIO (runExceptT $ PG.runTx appEnvMetadataDbPool (PG.ReadCommitted, Nothing) $ getPgVersion)
      `onLeftM` throwErrJExit DatabaseMigrationError

  lift . unLoggerTracing logger $ mkGenericLog @Text LevelInfo "telemetry" telemetryNotice

  computeResources <- getServerResources

  -- start a background thread for telemetry
  _telemetryThread <-
    C.forkManagedT "runTelemetry" logger
      $ runTelemetry logger appStateRef dbUid pgVersion computeResources

  -- forking a dedicated polling thread to dynamically get the latest JWK settings
  -- set by the user and update the JWK accordingly. This will help in applying the
  -- updates without restarting HGE.
  _ <-
    C.forkManagedT "update JWK" logger
      $ updateJwkCtxThread (getAppContext appStateRef) appEnvManager logger

  -- These cleanup actions are not directly associated with any
  -- resource, but we still need to make sure we clean them up here.
  allocate_ (pure ()) (liftIO stopWsServer)

  pure app
  where
    isRetryRequired _ resp = do
      return $ case resp of
        Right _ -> False
        Left err -> qeCode err == ConcurrentUpdate

    getLatestConfigForWSServer =
      fmap
        (\appCtx -> (acAuthMode appCtx, acEnableAllowlist appCtx, acCorsPolicy appCtx, acSQLGenCtx appCtx, acExperimentalFeatures appCtx, acDefaultNamingConvention appCtx))
        (getAppContext appStateRef)
    getSchemaCache' = getSchemaCache appStateRef

    prepareScheduledEvents (LoggerTracing logger) = do
      logger $ mkGenericLog @Text LevelInfo "scheduled_triggers" "preparing data"
      res <- Retry.retrying Retry.retryPolicyDefault isRetryRequired (return unlockAllLockedScheduledEvents)
      onLeft res (\err -> logger $ mkGenericLog @String LevelError "scheduled_triggers" (show $ qeError err))

    getProcessingScheduledEventsCount :: LockedEventsCtx -> IO Int
    getProcessingScheduledEventsCount LockedEventsCtx {..} = do
      processingCronEvents <- readTVarIO leCronEvents
      processingOneOffEvents <- readTVarIO leOneOffEvents
      return $ length processingOneOffEvents + length processingCronEvents

    shutdownEventTriggerEvents ::
      [BackendSourceInfo] ->
      Logger Hasura ->
      LockedEventsCtx ->
      IO ()
    shutdownEventTriggerEvents sources (Logger logger) LockedEventsCtx {..} = do
      -- TODO: is this correct?
      -- event triggers should be tied to the life cycle of a source
      lockedEvents <- readTVarIO leEvents
      forM_ sources $ \backendSourceInfo -> do
        AB.dispatchAnyBackend @BackendEventTrigger backendSourceInfo \(SourceInfo {..} :: SourceInfo b) -> do
          let sourceNameText = sourceNameToText _siName
          logger $ mkGenericLog LevelInfo "event_triggers" $ "unlocking events of source: " <> sourceNameText
          for_ (HashMap.lookup _siName lockedEvents) $ \sourceLockedEvents -> do
            -- No need to execute unlockEventsTx when events are not present
            for_ (NE.nonEmptySet sourceLockedEvents) $ \nonEmptyLockedEvents -> do
              res <- Retry.retrying Retry.retryPolicyDefault isRetryRequired (return $ unlockEventsInSource @b _siConfiguration nonEmptyLockedEvents)
              case res of
                Left err ->
                  logger
                    $ mkGenericLog LevelWarn "event_trigger"
                    $ "Error while unlocking event trigger events of source: "
                    <> sourceNameText
                    <> " error:"
                    <> showQErr err
                Right count ->
                  logger
                    $ mkGenericLog LevelInfo "event_trigger"
                    $ tshow count
                    <> " events of source "
                    <> sourceNameText
                    <> " were successfully unlocked"

    shutdownAsyncActions ::
      LockedEventsCtx ->
      ExceptT QErr m ()
    shutdownAsyncActions lockedEventsCtx = do
      lockedActionEvents <- liftIO $ readTVarIO $ leActionEvents lockedEventsCtx
      liftEitherM $ setProcessingActionLogsToPending (LockedActionIdArray $ toList lockedActionEvents)

    -- This function is a helper function to do couple of things:
    --
    -- 1. When the value of the `graceful-shutdown-timeout` > 0, we poll
    --    the in-flight events queue we maintain using the `processingEventsCountAction`
    --    number of in-flight processing events, in case of actions it is the
    --    actions which are in 'processing' state and in scheduled events
    --    it is the events which are in 'locked' state. The in-flight events queue is polled
    --    every 5 seconds until either the graceful shutdown time is exhausted
    --    or the number of in-flight processing events is 0.
    -- 2. After step 1, we unlock all the events which were attempted to process by the current
    --    graphql-engine instance that are still in the processing
    --    state. In actions, it means to set the status of such actions to 'pending'
    --    and in scheduled events, the status will be set to 'unlocked'.
    waitForProcessingAction ::
      Logger Hasura ->
      String ->
      IO Int ->
      ShutdownAction ->
      Seconds ->
      IO ()
    waitForProcessingAction l@(Logger logger) actionType processingEventsCountAction' shutdownAction maxTimeout
      | maxTimeout <= 0 = do
          case shutdownAction of
            EventTriggerShutdownAction userDBShutdownAction -> userDBShutdownAction
            MetadataDBShutdownAction metadataDBShutdownAction ->
              runExceptT metadataDBShutdownAction >>= \case
                Left err ->
                  logger
                    $ mkGenericLog LevelWarn (T.pack actionType)
                    $ "Error while unlocking the processing  "
                    <> tshow actionType
                    <> " err - "
                    <> showQErr err
                Right () -> pure ()
      | otherwise = do
          processingEventsCount <- processingEventsCountAction'
          if (processingEventsCount == 0)
            then
              logger
                $ mkGenericLog @Text LevelInfo (T.pack actionType)
                $ "All in-flight events have finished processing"
            else unless (processingEventsCount == 0) $ do
              C.sleep (5) -- sleep for 5 seconds and then repeat
              waitForProcessingAction l actionType processingEventsCountAction' shutdownAction (maxTimeout - (Seconds 5))

    startEventTriggerPollerThread logger lockedEventsCtx = do
      AppEnv {..} <- lift askAppEnv
      schemaCache <- liftIO $ getSchemaCache appStateRef
      let allSources = HashMap.elems $ scSources schemaCache
      activeEventProcessingThreads <- liftIO $ newTVarIO 0
      appCtx <- liftIO $ getAppContext appStateRef
      let fetchInterval = _eeCtxFetchInterval $ acEventEngineCtx appCtx
          fetchBatchSize = _eeCtxFetchSize $ acEventEngineCtx appCtx
      unless (unrefine fetchBatchSize == 0 || fetchInterval == 0) $ do
        -- Initialise the event processing thread
        let eventsGracefulShutdownAction =
              waitForProcessingAction
                logger
                "event_triggers"
                (length <$> readTVarIO (leEvents lockedEventsCtx))
                (EventTriggerShutdownAction (shutdownEventTriggerEvents allSources logger lockedEventsCtx))
                (unrefine appEnvGracefulShutdownTimeout)

        -- Create logger for logging the statistics of events fetched
        fetchedEventsStatsLogger <-
          allocate
            (createFetchedEventsStatsLogger logger)
            (closeFetchedEventsStatsLogger logger)

        lift $ unLoggerTracing logger $ mkGenericLog @Text LevelInfo "event_triggers" "starting workers"
        void
          $ C.forkManagedTWithGracefulShutdown
            "processEventQueue"
            logger
            (C.ThreadShutdown (liftIO eventsGracefulShutdownAction))
          $ processEventQueue
            logger
            fetchedEventsStatsLogger
            appEnvManager
            (getSchemaCache appStateRef)
            (acEventEngineCtx <$> getAppContext appStateRef)
            activeEventProcessingThreads
            lockedEventsCtx
            appEnvServerMetrics
            (pmEventTriggerMetrics appEnvPrometheusMetrics)
            appEnvEnableMaintenanceMode
            appEnvTriggersErrorLogLevelStatus

    startAsyncActionsPollerThread logger lockedEventsCtx actionSubState = do
      AppEnv {..} <- lift askAppEnv
      let label = "asyncActionsProcessor"
          asyncActionGracefulShutdownAction =
            ( liftWithStateless \lowerIO ->
                ( waitForProcessingAction
                    logger
                    "async_actions"
                    (length <$> readTVarIO (leActionEvents lockedEventsCtx))
                    (MetadataDBShutdownAction (hoist lowerIO (shutdownAsyncActions lockedEventsCtx)))
                    (unrefine appEnvGracefulShutdownTimeout)
                )
            )

      -- start a background thread to handle async actions
      void
        $ C.forkManagedTWithGracefulShutdown
          label
          logger
          (C.ThreadShutdown asyncActionGracefulShutdownAction)
        $ asyncActionsProcessor
          (acEnvironment <$> getAppContext appStateRef)
          logger
          (getSchemaCache appStateRef)
          (acAsyncActionsFetchInterval <$> getAppContext appStateRef)
          (leActionEvents lockedEventsCtx)
          Nothing
          appEnvAsyncActionsFetchBatchSize

      -- start a background thread to handle async action live queries
      void
        $ C.forkManagedT "asyncActionSubscriptionsProcessor" logger
        $ asyncActionSubscriptionsProcessor actionSubState

    startScheduledEventsPollerThread logger lockedEventsCtx = do
      AppEnv {..} <- lift askAppEnv
      -- prepare scheduled triggers
      lift $ prepareScheduledEvents logger

      -- Create logger for logging the statistics of scheduled events fetched
      scheduledEventsStatsLogger <-
        allocate
          (createFetchedScheduledEventsStatsLogger logger)
          (closeFetchedScheduledEventsStatsLogger logger)

      -- start a background thread to deliver the scheduled events
      -- _scheduledEventsThread <- do
      let scheduledEventsGracefulShutdownAction =
            ( liftWithStateless \lowerIO ->
                ( waitForProcessingAction
                    logger
                    "scheduled_events"
                    (getProcessingScheduledEventsCount lockedEventsCtx)
                    (MetadataDBShutdownAction (liftEitherM $ hoist lowerIO unlockAllLockedScheduledEvents))
                    (unrefine appEnvGracefulShutdownTimeout)
                )
            )

      void
        $ C.forkManagedTWithGracefulShutdown
          "processScheduledTriggers"
          logger
          (C.ThreadShutdown scheduledEventsGracefulShutdownAction)
        $ processScheduledTriggers
          (acEnvironment <$> getAppContext appStateRef)
          logger
          scheduledEventsStatsLogger
          appEnvManager
          (pmScheduledTriggerMetrics appEnvPrometheusMetrics)
          (getSchemaCache appStateRef)
          lockedEventsCtx
          appEnvTriggersErrorLogLevelStatus

runInSeparateTx ::
  PG.TxE QErr a ->
  AppM (Either QErr a)
runInSeparateTx tx = do
  pool <- asks appEnvMetadataDbPool
  liftIO $ runExceptT $ PG.runTx pool (PG.RepeatableRead, Nothing) tx

notifySchemaCacheSyncTx :: MetadataResourceVersion -> InstanceId -> CacheInvalidations -> PG.TxE QErr ()
notifySchemaCacheSyncTx (MetadataResourceVersion resourceVersion) instanceId invalidations = do
  PG.Discard () <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
      INSERT INTO hdb_catalog.hdb_schema_notifications(id, notification, resource_version, instance_id)
      VALUES (1, $1::json, $2, $3::uuid)
      ON CONFLICT (id) DO UPDATE SET
        notification = $1::json,
        resource_version = $2,
        instance_id = $3::uuid
    |]
      (PG.ViaJSON invalidations, resourceVersion, instanceId)
      True
  pure ()

getCatalogStateTx :: PG.TxE QErr CatalogState
getCatalogStateTx =
  mkCatalogState
    . PG.getRow
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
    SELECT hasura_uuid::text, cli_state::json, console_state::json
      FROM hdb_catalog.hdb_version
  |]
      ()
      False
  where
    mkCatalogState (dbId, PG.ViaJSON cliState, PG.ViaJSON consoleState) =
      CatalogState dbId cliState consoleState

setCatalogStateTx :: CatalogStateType -> J.Value -> PG.TxE QErr ()
setCatalogStateTx stateTy stateValue =
  case stateTy of
    CSTCli ->
      PG.unitQE
        defaultTxErrorHandler
        [PG.sql|
        UPDATE hdb_catalog.hdb_version
           SET cli_state = $1
      |]
        (Identity $ PG.ViaJSON stateValue)
        False
    CSTConsole ->
      PG.unitQE
        defaultTxErrorHandler
        [PG.sql|
        UPDATE hdb_catalog.hdb_version
           SET console_state = $1
      |]
        (Identity $ PG.ViaJSON stateValue)
        False

--- helper functions ---

mkConsoleHTML ::
  Text ->
  AuthMode ->
  TelemetryStatus ->
  Maybe Text ->
  Maybe Text ->
  CEConsoleType ->
  Either String Text
mkConsoleHTML path authMode enableTelemetry consoleAssetsDir consoleSentryDsn ceConsoleType =
  renderHtmlTemplate consoleTmplt
    $
    -- variables required to render the template
    J.object
      [ "isAdminSecretSet" J..= isAdminSecretSet authMode,
        "consolePath" J..= consolePath,
        "enableTelemetry" J..= boolToText (isTelemetryEnabled enableTelemetry),
        "cdnAssets" J..= boolToText (isNothing consoleAssetsDir),
        "consoleSentryDsn" J..= fromMaybe "" consoleSentryDsn,
        "assetsVersion" J..= consoleAssetsVersion,
        "serverVersion" J..= currentVersion,
        "consoleType" J..= ceConsoleTypeIdentifier ceConsoleType, -- TODO(awjchen): This is a kludge that will be removed when the entitlement service is fully implemented.
        "consoleSentryDsn" J..= ("" :: Text)
      ]
  where
    consolePath = case path of
      "" -> "/console"
      r -> "/console/" <> r

    consoleTmplt = $(makeRelativeToProject "src-rsr/console.html" >>= M.embedSingleTemplate)

telemetryNotice :: Text
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
    <> "usage stats which allows us to keep improving Hasura at warp speed. "
    <> "To read more or opt-out, visit https://hasura.io/docs/latest/graphql/core/guides/telemetry.html"

mkPgSourceResolver :: PG.PGLogger -> SourceResolver ('Postgres 'Vanilla)
mkPgSourceResolver pgLogger env sourceName config = runExceptT do
  let PostgresSourceConnInfo urlConf poolSettings allowPrepare isoLevel _ = pccConnectionInfo config
  -- If the user does not provide values for the pool settings, then use the default values
  let (maxConns, idleTimeout, retries) = getDefaultPGPoolSettingIfNotExists poolSettings defaultPostgresPoolSettings
  connDetails <- resolveUrlConf env urlConf
  let connInfo = PG.ConnInfo retries connDetails
      connParams =
        PG.defaultConnParams
          { PG.cpIdleTime = idleTimeout,
            PG.cpConns = maxConns,
            PG.cpAllowPrepare = allowPrepare,
            PG.cpMbLifetime = ppsConnectionLifetime =<< poolSettings,
            PG.cpTimeout = ppsPoolTimeout =<< poolSettings
          }
  let context = J.object [("source" J..= sourceName)]
  pgPool <- liftIO $ Q.initPGPool connInfo context connParams pgLogger
  let pgExecCtx = mkPGExecCtx isoLevel pgPool NeverResizePool
  pure $ PGSourceConfig pgExecCtx connInfo Nothing mempty (pccExtensionsSchema config) mempty ConnTemplate_NotApplicable

mkMSSQLSourceResolver :: SourceResolver 'MSSQL
mkMSSQLSourceResolver env _name (MSSQLConnConfiguration connInfo _) = runExceptT do
  let MSSQLConnectionInfo iConnString poolSettings isolationLevel = connInfo
      connOptions = case poolSettings of
        MSSQLPoolSettingsPool (MSSQLPoolConnectionSettings {..}) ->
          MSPool.ConnectionOptionsPool
            $ MSPool.PoolOptions
              { poConnections = fromMaybe defaultMSSQLMaxConnections mpsMaxConnections,
                poStripes = 1,
                poIdleTime = mpsIdleTimeout
              }
        MSSQLPoolSettingsNoPool -> MSPool.ConnectionOptionsNoPool
  (connString, mssqlPool) <- createMSSQLPool iConnString connOptions env
  let mssqlExecCtx = mkMSSQLExecCtx isolationLevel mssqlPool NeverResizePool
      numReadReplicas = 0
  pure $ MSSQLSourceConfig connString mssqlExecCtx numReadReplicas
