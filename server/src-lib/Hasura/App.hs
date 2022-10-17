{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Imported by 'server/src-exec/Main.hs'.
module Hasura.App
  ( ExitCode (DatabaseMigrationError, DowngradeProcessError, MetadataCleanError, MetadataExportError, SchemaCacheInitError),
    ExitException (ExitException),
    GlobalCtx (..),
    Loggers (..),
    PGMetadataStorageAppT (runPGMetadataStorageAppT),
    ServeCtx (ServeCtx, _scLoggers, _scMetadataDbPool, _scShutdownLatch),
    ShutdownLatch,
    accessDeniedErrMsg,
    flushLogger,
    getCatalogStateTx,
    initGlobalCtx,
    initialiseServeCtx,
    migrateCatalogSchema,
    mkLoggers,
    mkPGLogger,
    newShutdownLatch,
    notifySchemaCacheSyncTx,
    parseArgs,
    throwErrExit,
    throwErrJExit,
    printJSON,
    printYaml,
    readTlsAllowlist,
    resolvePostgresConnInfo,
    runHGEServer,
    setCatalogStateTx,
    shutdownGracefully,
    waitForShutdown,
    shuttingDown,

    -- * Exported for testing
    mkHGEServer,
    mkPgSourceResolver,
    mkMSSQLSourceResolver,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.Extended qualified as C
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Exception (bracket_, throwIO)
import Control.Monad.Catch
  ( Exception,
    MonadCatch,
    MonadMask,
    MonadThrow,
    onException,
  )
import Control.Monad.Morph (hoist)
import Control.Monad.STM (atomically)
import Control.Monad.Stateless
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Managed (ManagedT (..), allocate_)
import Control.Retry qualified as Retry
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Environment qualified as Env
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict qualified as HM
import Data.Set.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import Data.Yaml qualified as Y
import Database.MSSQL.Pool qualified as MSPool
import Database.PG.Query qualified as PG
import Database.PG.Query qualified as Q
import GHC.AssertNF.CPP
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
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
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.Subscription.Poll qualified as ES
import Hasura.GraphQL.Logging (MonadQueryLog (..))
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Transport.HTTP
  ( CacheStoreSuccess (CacheStoreSkipped),
    MonadExecuteQuery (..),
  )
import Hasura.GraphQL.Transport.HTTP.Protocol (toParsed)
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup (..))
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Catalog
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Eventing.Backend
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.API.Query (requiresAdmin)
import Hasura.Server.App
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
import Hasura.Server.SchemaCacheRef
  ( SchemaCacheRef,
    getSchemaCache,
    initialiseSchemaCacheRef,
    logInconsistentMetadata,
  )
import Hasura.Server.SchemaUpdate
import Hasura.Server.Telemetry
import Hasura.Server.Types
import Hasura.Server.Version
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Blocklisting (Blocklist)
import Network.HTTP.Client.CreateManager (mkHttpManager)
import Network.HTTP.Client.Manager (HasHttpManagerM (..))
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import Refined (unrefine)
import System.Environment (getEnvironment)
import System.Log.FastLogger qualified as FL
import System.Metrics qualified as EKG
import System.Metrics.Gauge qualified as EKG.Gauge
import Text.Mustache.Compile qualified as M
import Web.Spock.Core qualified as Spock

data ExitCode
  = -- these are used during server initialization:
    InvalidEnvironmentVariableOptionsError
  | InvalidDatabaseConnectionParamsError
  | AuthConfigurationError
  | EventSubSystemError
  | DatabaseMigrationError
  | -- | used by MT because it initialises the schema cache only
    -- these are used in app/Main.hs:
    SchemaCacheInitError
  | MetadataExportError
  | MetadataCleanError
  | ExecuteProcessError
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

throwErrJExit :: (A.ToJSON a, MonadIO m) => forall b. ExitCode -> a -> m b
throwErrJExit reason = liftIO . throwIO . ExitException reason . BLC.toStrict . A.encode

--------------------------------------------------------------------------------
-- TODO(SOLOMON): Move Into `Hasura.Server.Init`. Unable to do so
-- currently due `throwErrExit`.

-- | Parse cli arguments to graphql-engine executable.
parseArgs :: EnabledLogTypes impl => IO (HGEOptions (ServeOptions impl))
parseArgs = do
  rawHGEOpts <- execParser opts
  env <- getEnvironment
  let eitherOpts = runWithEnv env $ mkHGEOptions rawHGEOpts
  onLeft eitherOpts $ throwErrExit InvalidEnvironmentVariableOptionsError
  where
    opts =
      info
        (helper <*> parseHgeOpts)
        ( fullDesc
            <> header "Hasura GraphQL Engine: Blazing fast, instant realtime GraphQL APIs on your DB with fine grained access control, also trigger webhooks on database events."
            <> footerDoc (Just mainCmdFooter)
        )

--------------------------------------------------------------------------------

printJSON :: (A.ToJSON a, MonadIO m) => a -> m ()
printJSON = liftIO . BLC.putStrLn . A.encode

printYaml :: (A.ToJSON a, MonadIO m) => a -> m ()
printYaml = liftIO . BC.putStrLn . Y.encode

mkPGLogger :: Logger Hasura -> PG.PGLogger
mkPGLogger (Logger logger) (PG.PLERetryMsg msg) =
  logger $ PGLog LevelWarn msg

-- | Context required for all graphql-engine CLI commands
data GlobalCtx = GlobalCtx
  { _gcMetadataDbConnInfo :: !PG.ConnInfo,
    -- | --database-url option, @'UrlConf' is required to construct default source configuration
    -- and optional retries
    _gcDefaultPostgresConnInfo :: !(Maybe (UrlConf, PG.ConnInfo), Maybe Int)
  }

readTlsAllowlist :: SchemaCacheRef -> IO [TlsAllow]
readTlsAllowlist scRef = scTlsAllowlist <$> getSchemaCache scRef

initGlobalCtx ::
  (MonadIO m) =>
  Env.Environment ->
  -- | the metadata DB URL
  Maybe String ->
  -- | the user's DB URL
  PostgresConnInfo (Maybe UrlConf) ->
  m GlobalCtx
initGlobalCtx env metadataDbUrl defaultPgConnInfo = do
  let PostgresConnInfo dbUrlConf maybeRetries = defaultPgConnInfo
      mkConnInfoFromSource dbUrl = do
        resolvePostgresConnInfo env dbUrl maybeRetries

      mkConnInfoFromMDb mdbUrl =
        let retries = fromMaybe 1 maybeRetries
         in (PG.ConnInfo retries . PG.CDDatabaseURI . txtToBs . T.pack) mdbUrl

      mkGlobalCtx mdbConnInfo sourceConnInfo =
        pure $ GlobalCtx mdbConnInfo (sourceConnInfo, maybeRetries)

  case (metadataDbUrl, dbUrlConf) of
    (Nothing, Nothing) ->
      throwErrExit
        InvalidDatabaseConnectionParamsError
        "Fatal Error: Either of --metadata-database-url or --database-url option expected"
    -- If no metadata storage specified consider use default database as
    -- metadata storage
    (Nothing, Just dbUrl) -> do
      connInfo <- mkConnInfoFromSource dbUrl
      mkGlobalCtx connInfo $ Just (dbUrl, connInfo)
    (Just mdUrl, Nothing) -> do
      let mdConnInfo = mkConnInfoFromMDb mdUrl
      mkGlobalCtx mdConnInfo Nothing
    (Just mdUrl, Just dbUrl) -> do
      srcConnInfo <- mkConnInfoFromSource dbUrl
      let mdConnInfo = mkConnInfoFromMDb mdUrl
      mkGlobalCtx mdConnInfo (Just (dbUrl, srcConnInfo))

-- | Context required for the 'serve' CLI command.
data ServeCtx = ServeCtx
  { _scHttpManager :: !HTTP.Manager,
    _scInstanceId :: !InstanceId,
    _scLoggers :: !Loggers,
    _scEnabledLogTypes :: !(HashSet (EngineLogType Hasura)),
    _scMetadataDbPool :: !PG.PGPool,
    _scShutdownLatch :: !ShutdownLatch,
    _scSchemaCache :: !RebuildableSchemaCache,
    _scSchemaCacheRef :: !SchemaCacheRef,
    _scMetaVersionRef :: !(STM.TMVar MetadataResourceVersion)
  }

-- | Collection of the LoggerCtx, the regular Logger and the PGLogger
-- TODO (from master): better naming?
data Loggers = Loggers
  { _lsLoggerCtx :: !(LoggerCtx Hasura),
    _lsLogger :: !(Logger Hasura),
    _lsPgLogger :: !PG.PGLogger
  }

-- | An application with Postgres database as a metadata storage
newtype PGMetadataStorageAppT m a = PGMetadataStorageAppT {runPGMetadataStorageAppT :: (PG.PGPool, PG.PGLogger) -> m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadCatch,
      MonadThrow,
      MonadMask,
      HasHttpManagerM,
      HasServerConfigCtx,
      MonadReader (PG.PGPool, PG.PGLogger),
      MonadBase b,
      MonadBaseControl b
    )
    via (ReaderT (PG.PGPool, PG.PGLogger) m)
  deriving
    ( MonadTrans
    )
    via (ReaderT (PG.PGPool, PG.PGLogger))

resolvePostgresConnInfo ::
  (MonadIO m) => Env.Environment -> UrlConf -> Maybe Int -> m PG.ConnInfo
resolvePostgresConnInfo env dbUrlConf maybeRetries = do
  dbUrlText <-
    runExcept (resolveUrlConf env dbUrlConf) `onLeft` \err ->
      liftIO (throwErrJExit InvalidDatabaseConnectionParamsError err)
  pure $ PG.ConnInfo retries $ PG.CDDatabaseURI $ txtToBs dbUrlText
  where
    retries = fromMaybe 1 maybeRetries

-- | Initializes or migrates the catalog and returns the context required to start the server.
initialiseServeCtx ::
  (C.ForkableMonadIO m, MonadCatch m) =>
  Env.Environment ->
  GlobalCtx ->
  ServeOptions Hasura ->
  ServerMetrics ->
  ManagedT m ServeCtx
initialiseServeCtx env GlobalCtx {..} so@ServeOptions {..} serverMetrics = do
  instanceId <- liftIO generateInstanceId
  latch <- liftIO newShutdownLatch
  loggers@(Loggers loggerCtx logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel
  when (null soAdminSecret) $ do
    let errMsg :: Text
        errMsg = "WARNING: No admin secret provided"
    unLogger logger $
      StartupLog
        { slLogLevel = LevelWarn,
          slKind = "no_admin_secret",
          slInfo = A.toJSON errMsg
        }
  -- log serve options
  unLogger logger $ serveOptsToLog so

  -- log postgres connection info
  unLogger logger $ connInfoToLog _gcMetadataDbConnInfo

  metadataDbPool <- liftIO $ PG.initPGPool _gcMetadataDbConnInfo soConnParams pgLogger

  let maybeDefaultSourceConfig =
        fst _gcDefaultPostgresConnInfo <&> \(dbUrlConf, _) ->
          let connSettings =
                PostgresPoolSettings
                  { _ppsMaxConnections = Just $ Q.cpConns soConnParams,
                    _ppsTotalMaxConnections = Nothing,
                    _ppsIdleTimeout = Just $ Q.cpIdleTime soConnParams,
                    _ppsRetries = snd _gcDefaultPostgresConnInfo <|> Just 1,
                    _ppsPoolTimeout = PG.cpTimeout soConnParams,
                    _ppsConnectionLifetime = PG.cpMbLifetime soConnParams
                  }
              sourceConnInfo = PostgresSourceConnInfo dbUrlConf (Just connSettings) (PG.cpAllowPrepare soConnParams) soTxIso Nothing
           in PostgresConnConfiguration sourceConnInfo Nothing defaultPostgresExtensionsSchema
      optimizePermissionFilters
        | EFOptimizePermissionFilters `elem` soExperimentalFeatures = Options.OptimizePermissionFilters
        | otherwise = Options.Don'tOptimizePermissionFilters

      bigqueryStringNumericInput
        | EFBigQueryStringNumericInput `elem` soExperimentalFeatures = Options.EnableBigQueryStringNumericInput
        | otherwise = Options.DisableBigQueryStringNumericInput
      sqlGenCtx = SQLGenCtx soStringifyNum soDangerousBooleanCollapse optimizePermissionFilters bigqueryStringNumericInput

  let serverConfigCtx =
        ServerConfigCtx
          soInferFunctionPermissions
          soEnableRemoteSchemaPermissions
          sqlGenCtx
          soEnableMaintenanceMode
          soExperimentalFeatures
          soEventingMode
          soReadOnlyMode
          soDefaultNamingConvention

  rebuildableSchemaCache <-
    lift . flip onException (flushLogger loggerCtx) $
      migrateCatalogSchema
        env
        logger
        metadataDbPool
        maybeDefaultSourceConfig
        mempty
        serverConfigCtx
        (mkPgSourceResolver pgLogger)
        mkMSSQLSourceResolver
        soExtensionsSchema

  -- Start a background thread for listening schema sync events from other server instances,
  metaVersionRef <- liftIO $ STM.newEmptyTMVarIO

  -- An interval of 0 indicates that no schema sync is required
  case soSchemaPollInterval of
    Skip -> unLogger logger $ mkGenericStrLog LevelInfo "schema-sync" "Schema sync disabled"
    Interval interval -> do
      unLogger logger $ mkGenericStrLog LevelInfo "schema-sync" ("Schema sync enabled. Polling at " <> show interval)
      void $ startSchemaSyncListenerThread logger metadataDbPool instanceId interval metaVersionRef

  schemaCacheRef <- initialiseSchemaCacheRef serverMetrics rebuildableSchemaCache

  srvMgr <- liftIO $ mkHttpManager (readTlsAllowlist schemaCacheRef) mempty

  pure $
    ServeCtx
      srvMgr
      instanceId
      loggers
      soEnabledLogTypes
      metadataDbPool
      latch
      rebuildableSchemaCache
      schemaCacheRef
      metaVersionRef

mkLoggers ::
  (MonadIO m, MonadBaseControl IO m) =>
  HashSet (EngineLogType Hasura) ->
  LogLevel ->
  ManagedT m Loggers
mkLoggers enabledLogs logLevel = do
  loggerCtx <- mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
  let logger = mkLogger loggerCtx
      pgLogger = mkPGLogger logger
  return $ Loggers loggerCtx logger pgLogger

-- | helper function to initialize or migrate the @hdb_catalog@ schema (used by pro as well)
migrateCatalogSchema ::
  (MonadIO m, MonadBaseControl IO m) =>
  Env.Environment ->
  Logger Hasura ->
  PG.PGPool ->
  Maybe (SourceConnConfiguration ('Postgres 'Vanilla)) ->
  Blocklist ->
  ServerConfigCtx ->
  SourceResolver ('Postgres 'Vanilla) ->
  SourceResolver ('MSSQL) ->
  ExtensionsSchema ->
  m RebuildableSchemaCache
migrateCatalogSchema
  env
  logger
  pool
  defaultSourceConfig
  blockList
  serverConfigCtx
  pgSourceResolver
  mssqlSourceResolver
  extensionsSchema = do
    initialiseResult <- runExceptT $ do
      -- TODO: should we allow the migration to happen during maintenance mode?
      -- Allowing this can be a sanity check, to see if the hdb_catalog in the
      -- DB has been set correctly
      currentTime <- liftIO Clock.getCurrentTime
      (migrationResult, metadata) <-
        PG.runTx pool (PG.Serializable, Just PG.ReadWrite) $
          migrateCatalog
            defaultSourceConfig
            extensionsSchema
            (_sccMaintenanceMode serverConfigCtx)
            currentTime
      let tlsAllowList = networkTlsAllowlist $ _metaNetwork metadata
      httpManager <- liftIO $ mkHttpManager (pure tlsAllowList) blockList
      let cacheBuildParams =
            CacheBuildParams httpManager pgSourceResolver mssqlSourceResolver serverConfigCtx
          buildReason = CatalogSync
      schemaCache <-
        runCacheBuild cacheBuildParams $
          buildRebuildableSchemaCacheWithReason buildReason logger env metadata
      pure (migrationResult, schemaCache)

    (migrationResult, schemaCache) <-
      initialiseResult `onLeft` \err -> do
        unLogger
          logger
          StartupLog
            { slLogLevel = LevelError,
              slKind = "catalog_migrate",
              slInfo = A.toJSON err
            }
        liftIO (throwErrJExit DatabaseMigrationError err)
    unLogger logger migrationResult
    pure schemaCache

-- | A latch for the graceful shutdown of a server process.
newtype ShutdownLatch = ShutdownLatch {unShutdownLatch :: C.MVar ()}

-- | Event triggers live in the user's DB and other events
--  (cron, one-off and async actions)
--   live in the metadata DB, so we need a way to differentiate the
--   type of shutdown action
data ShutdownAction
  = EventTriggerShutdownAction (IO ())
  | MetadataDBShutdownAction (MetadataStorageT IO ())

newShutdownLatch :: IO ShutdownLatch
newShutdownLatch = fmap ShutdownLatch C.newEmptyMVar

-- | Block the current thread, waiting on the latch.
waitForShutdown :: ShutdownLatch -> IO ()
waitForShutdown = C.readMVar . unShutdownLatch

-- | Initiate a graceful shutdown of the server associated with the provided
-- latch.
shutdownGracefully :: ShutdownLatch -> IO ()
shutdownGracefully = void . flip C.tryPutMVar () . unShutdownLatch

-- | Returns True if the latch is set for shutdown and vice-versa
shuttingDown :: ShutdownLatch -> IO Bool
shuttingDown latch = not <$> C.isEmptyMVar (unShutdownLatch latch)

-- | If an exception is encountered , flush the log buffer and
-- rethrow If we do not flush the log buffer on exception, then log lines
-- may be missed
-- See: https://github.com/hasura/graphql-engine/issues/4772
flushLogger :: MonadIO m => LoggerCtx impl -> m ()
flushLogger = liftIO . FL.flushLogStr . _lcLoggerSet

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
runHGEServer ::
  forall m impl.
  ( MonadIO m,
    MonadFix m,
    MonadMask m,
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    UserAuthentication (Tracing.TraceT m),
    HttpLog m,
    ConsoleRenderer m,
    MonadMetadataApiAuthorization m,
    MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    WS.MonadWSLog m,
    MonadExecuteQuery m,
    Tracing.HasReporter m,
    HasResourceLimits m,
    MonadMetadataStorage (MetadataStorageT m),
    MonadResolveSource m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m
  ) =>
  (ServerCtx -> Spock.SpockT m ()) ->
  Env.Environment ->
  ServeOptions impl ->
  ServeCtx ->
  -- and mutations

  -- | start time
  UTCTime ->
  Maybe ES.SubscriptionPostPollHook ->
  ServerMetrics ->
  EKG.Store EKG.EmptyMetrics ->
  -- | A hook which can be called to indicate when the server is started succesfully
  Maybe (IO ()) ->
  PrometheusMetrics ->
  ManagedT m ()
runHGEServer setupHook env serveOptions serveCtx initTime postPollHook serverMetrics ekgStore startupStatusHook prometheusMetrics = do
  waiApplication <-
    mkHGEServer setupHook env serveOptions serveCtx initTime postPollHook serverMetrics ekgStore prometheusMetrics

  -- `startupStatusHook`: add `Service started successfully` message to config_status
  -- table when a tenant starts up in multitenant
  let warpSettings :: Warp.Settings
      warpSettings =
        Warp.setPort (_getPort $ soPort serveOptions)
          . Warp.setHost (soHost serveOptions)
          . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
          . Warp.setInstallShutdownHandler shutdownHandler
          . Warp.setBeforeMainLoop (for_ startupStatusHook id)
          . setForkIOWithMetrics
          $ Warp.defaultSettings

      setForkIOWithMetrics :: Warp.Settings -> Warp.Settings
      setForkIOWithMetrics = Warp.setFork \f -> do
        void $
          C.forkIOWithUnmask
            ( \unmask ->
                bracket_
                  ( do
                      EKG.Gauge.inc (smWarpThreads serverMetrics)
                      incWarpThreads (pmConnections prometheusMetrics)
                  )
                  ( do
                      EKG.Gauge.dec (smWarpThreads serverMetrics)
                      decWarpThreads (pmConnections prometheusMetrics)
                  )
                  (f unmask)
            )

      shutdownHandler :: IO () -> IO ()
      shutdownHandler closeSocket =
        LA.link =<< LA.async do
          waitForShutdown $ _scShutdownLatch serveCtx
          let logger = _lsLogger $ _scLoggers serveCtx
          unLogger logger $ mkGenericStrLog LevelInfo "server" "gracefully shutting down server"
          closeSocket

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
    MonadFix m,
    MonadMask m,
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    UserAuthentication (Tracing.TraceT m),
    HttpLog m,
    ConsoleRenderer m,
    MonadMetadataApiAuthorization m,
    MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    WS.MonadWSLog m,
    MonadExecuteQuery m,
    Tracing.HasReporter m,
    HasResourceLimits m,
    MonadMetadataStorage (MetadataStorageT m),
    MonadResolveSource m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m
  ) =>
  (ServerCtx -> Spock.SpockT m ()) ->
  Env.Environment ->
  ServeOptions impl ->
  ServeCtx ->
  -- and mutations

  -- | start time
  UTCTime ->
  Maybe ES.SubscriptionPostPollHook ->
  ServerMetrics ->
  EKG.Store EKG.EmptyMetrics ->
  PrometheusMetrics ->
  ManagedT m Application
mkHGEServer setupHook env ServeOptions {..} ServeCtx {..} initTime postPollHook serverMetrics ekgStore prometheusMetrics = do
  -- Comment this to enable expensive assertions from "GHC.AssertNF". These
  -- will log lines to STDOUT containing "not in normal form". In the future we
  -- could try to integrate this into our tests. For now this is a development
  -- tool.
  --
  -- NOTE: be sure to compile WITHOUT code coverage, for this to work properly.
  liftIO disableAssertNF

  let optimizePermissionFilters
        | EFOptimizePermissionFilters `elem` soExperimentalFeatures = Options.OptimizePermissionFilters
        | otherwise = Options.Don'tOptimizePermissionFilters

      bigqueryStringNumericInput
        | EFBigQueryStringNumericInput `elem` soExperimentalFeatures = Options.EnableBigQueryStringNumericInput
        | otherwise = Options.DisableBigQueryStringNumericInput

      sqlGenCtx = SQLGenCtx soStringifyNum soDangerousBooleanCollapse optimizePermissionFilters bigqueryStringNumericInput
      Loggers loggerCtx logger _ = _scLoggers

  authModeRes <-
    runExceptT $
      setupAuthMode
        soAdminSecret
        soAuthHook
        soJwtSecret
        soUnAuthRole
        _scHttpManager
        logger

  authMode <- onLeft authModeRes (throwErrExit AuthConfigurationError . T.unpack)

  HasuraApp app cacheRef actionSubState stopWsServer <-
    lift $
      flip onException (flushLogger loggerCtx) $
        mkWaiApp
          setupHook
          env
          logger
          sqlGenCtx
          soEnableAllowlist
          _scHttpManager
          authMode
          soCorsConfig
          soEnableConsole
          soConsoleAssetsDir
          soConsoleSentryDsn
          soEnableTelemetry
          _scInstanceId
          soEnabledAPIs
          soLiveQueryOpts
          soStreamingQueryOpts
          soResponseInternalErrorsConfig
          postPollHook
          _scSchemaCacheRef
          ekgStore
          serverMetrics
          prometheusMetrics
          soEnableRemoteSchemaPermissions
          soInferFunctionPermissions
          soConnectionOptions
          soWebSocketKeepAlive
          soEnableMaintenanceMode
          soEventingMode
          soReadOnlyMode
          soExperimentalFeatures
          _scEnabledLogTypes
          soWebSocketConnectionInitTimeout
          soEnableMetadataQueryLogging
          soDefaultNamingConvention

  let serverConfigCtx =
        ServerConfigCtx
          soInferFunctionPermissions
          soEnableRemoteSchemaPermissions
          sqlGenCtx
          soEnableMaintenanceMode
          soExperimentalFeatures
          soEventingMode
          soReadOnlyMode
          soDefaultNamingConvention

  -- Log Warning if deprecated environment variables are used
  sources <- scSources <$> liftIO (getSchemaCache cacheRef)
  liftIO $ logDeprecatedEnvVars logger env sources

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSchemaCache cacheRef)
  liftIO $ logInconsistentMetadata logger inconsObjs

  -- NOTE: `newLogTVar` is being used to make sure that the metadata logger runs only once
  --       while logging errors or any `inconsistent_metadata` logs.
  newLogTVar <- liftIO $ STM.newTVarIO False

  -- Start a background thread for processing schema sync event present in the '_sscSyncEventRef'
  _ <-
    startSchemaSyncProcessorThread
      logger
      _scHttpManager
      _scMetaVersionRef
      cacheRef
      _scInstanceId
      serverConfigCtx
      newLogTVar

  lockedEventsCtx <-
    liftIO $
      LockedEventsCtx
        <$> STM.newTVarIO mempty
        <*> STM.newTVarIO mempty
        <*> STM.newTVarIO mempty
        <*> STM.newTVarIO mempty

  case soEventingMode of
    EventingEnabled -> do
      startEventTriggerPollerThread logger lockedEventsCtx cacheRef
      startAsyncActionsPollerThread logger lockedEventsCtx cacheRef actionSubState

      -- start a background thread to create new cron events
      _cronEventsThread <-
        C.forkManagedT "runCronEventsGenerator" logger $
          runCronEventsGenerator logger (getSchemaCache cacheRef)

      startScheduledEventsPollerThread logger lockedEventsCtx cacheRef
    EventingDisabled ->
      unLogger logger $ mkGenericStrLog LevelInfo "server" "starting in eventing disabled mode"

  -- start a background thread to check for updates
  _updateThread <-
    C.forkManagedT "checkForUpdates" logger $
      liftIO $ checkForUpdates loggerCtx _scHttpManager

  -- start a background thread for telemetry
  _telemetryThread <-
    if soEnableTelemetry
      then do
        lift . unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice

        dbUid <-
          runMetadataStorageT getMetadataDbUid
            >>= (`onLeft` throwErrJExit DatabaseMigrationError)
        pgVersion <-
          liftIO (runExceptT $ PG.runTx _scMetadataDbPool (PG.ReadCommitted, Nothing) $ getPgVersion)
            >>= (`onLeft` throwErrJExit DatabaseMigrationError)

        telemetryThread <-
          C.forkManagedT "runTelemetry" logger $
            liftIO $ runTelemetry logger _scHttpManager (getSchemaCache cacheRef) dbUid _scInstanceId pgVersion
        return $ Just telemetryThread
      else return Nothing

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  unLogger logger $
    mkGenericLog LevelInfo "server" $ StartupTimeInfo "starting API server" apiInitTime

  -- These cleanup actions are not directly associated with any
  -- resource, but we still need to make sure we clean them up here.
  allocate_ (pure ()) (liftIO stopWsServer)

  pure app
  where
    isRetryRequired _ resp = do
      return $ case resp of
        Right _ -> False
        Left err -> qeCode err == ConcurrentUpdate

    prepareScheduledEvents (Logger logger) = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "scheduled_triggers" "preparing data"
      res <- Retry.retrying Retry.retryPolicyDefault isRetryRequired (return $ runMetadataStorageT unlockAllLockedScheduledEvents)
      onLeft res (\err -> logger $ mkGenericStrLog LevelError "scheduled_triggers" (show $ qeError err))

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
        AB.dispatchAnyBackend @BackendEventTrigger backendSourceInfo \(SourceInfo sourceName _ _ sourceConfig _ _ :: SourceInfo b) -> do
          let sourceNameText = sourceNameToText sourceName
          logger $ mkGenericLog LevelInfo "event_triggers" $ "unlocking events of source: " <> sourceNameText
          for_ (HM.lookup sourceName lockedEvents) $ \sourceLockedEvents -> do
            -- No need to execute unlockEventsTx when events are not present
            for_ (NE.nonEmptySet sourceLockedEvents) $ \nonEmptyLockedEvents -> do
              res <- Retry.retrying Retry.retryPolicyDefault isRetryRequired (return $ unlockEventsInSource @b sourceConfig nonEmptyLockedEvents)
              case res of
                Left err ->
                  logger $
                    mkGenericLog LevelWarn "event_trigger" $
                      "Error while unlocking event trigger events of source: " <> sourceNameText <> " error:" <> showQErr err
                Right count ->
                  logger $
                    mkGenericLog LevelInfo "event_trigger" $
                      tshow count <> " events of source " <> sourceNameText <> " were successfully unlocked"

    shutdownAsyncActions ::
      LockedEventsCtx ->
      MetadataStorageT m ()
    shutdownAsyncActions lockedEventsCtx = do
      lockedActionEvents <- liftIO $ readTVarIO $ leActionEvents lockedEventsCtx
      setProcessingActionLogsToPending (LockedActionIdArray $ toList lockedActionEvents)

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
            runMetadataStorageT metadataDBShutdownAction >>= \case
              Left err ->
                logger $
                  mkGenericLog LevelWarn (T.pack actionType) $
                    "Error while unlocking the processing  "
                      <> tshow actionType
                      <> " err - "
                      <> showQErr err
              Right () -> pure ()
      | otherwise = do
        processingEventsCount <- processingEventsCountAction'
        if (processingEventsCount == 0)
          then
            logger $
              mkGenericStrLog LevelInfo (T.pack actionType) $
                "All in-flight events have finished processing"
          else unless (processingEventsCount == 0) $ do
            C.sleep (5) -- sleep for 5 seconds and then repeat
            waitForProcessingAction l actionType processingEventsCountAction' shutdownAction (maxTimeout - (Seconds 5))

    startEventTriggerPollerThread logger lockedEventsCtx cacheRef = do
      let maxEventThreads = unrefine soEventsHttpPoolSize
          fetchInterval = milliseconds $ unrefine soEventsFetchInterval
          allSources = HM.elems $ scSources $ lastBuiltSchemaCache _scSchemaCache

      unless (unrefine soEventsFetchBatchSize == 0 || fetchInterval == 0) $ do
        -- Don't start the events poller thread when fetchBatchSize or fetchInterval is 0
        -- prepare event triggers data
        eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEventThreads fetchInterval soEventsFetchBatchSize
        let eventsGracefulShutdownAction =
              waitForProcessingAction
                logger
                "event_triggers"
                (length <$> readTVarIO (leEvents lockedEventsCtx))
                (EventTriggerShutdownAction (shutdownEventTriggerEvents allSources logger lockedEventsCtx))
                (unrefine soGracefulShutdownTimeout)
        unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"
        void $
          C.forkManagedTWithGracefulShutdown
            "processEventQueue"
            logger
            (C.ThreadShutdown (liftIO eventsGracefulShutdownAction))
            $ processEventQueue
              logger
              _scHttpManager
              (getSchemaCache cacheRef)
              eventEngineCtx
              lockedEventsCtx
              serverMetrics
              (pmEventTriggerMetrics prometheusMetrics)
              soEnableMaintenanceMode

    startAsyncActionsPollerThread logger lockedEventsCtx cacheRef actionSubState = do
      -- start a background thread to handle async actions
      case soAsyncActionsFetchInterval of
        Skip -> pure () -- Don't start the poller thread
        Interval (unrefine -> sleepTime) -> do
          let label = "asyncActionsProcessor"
              asyncActionGracefulShutdownAction =
                ( liftWithStateless \lowerIO ->
                    ( waitForProcessingAction
                        logger
                        "async_actions"
                        (length <$> readTVarIO (leActionEvents lockedEventsCtx))
                        (MetadataDBShutdownAction (hoist lowerIO (shutdownAsyncActions lockedEventsCtx)))
                        (unrefine soGracefulShutdownTimeout)
                    )
                )

          void $
            C.forkManagedTWithGracefulShutdown
              label
              logger
              (C.ThreadShutdown asyncActionGracefulShutdownAction)
              $ asyncActionsProcessor
                env
                logger
                (getSchemaCache cacheRef)
                (leActionEvents lockedEventsCtx)
                _scHttpManager
                sleepTime
                Nothing

      -- start a background thread to handle async action live queries
      void $
        C.forkManagedT "asyncActionSubscriptionsProcessor" logger $
          asyncActionSubscriptionsProcessor actionSubState

    startScheduledEventsPollerThread logger lockedEventsCtx cacheRef = do
      -- prepare scheduled triggers
      lift $ prepareScheduledEvents logger

      -- start a background thread to deliver the scheduled events
      -- _scheduledEventsThread <- do
      let scheduledEventsGracefulShutdownAction =
            ( liftWithStateless \lowerIO ->
                ( waitForProcessingAction
                    logger
                    "scheduled_events"
                    (getProcessingScheduledEventsCount lockedEventsCtx)
                    (MetadataDBShutdownAction (hoist lowerIO unlockAllLockedScheduledEvents))
                    (unrefine soGracefulShutdownTimeout)
                )
            )

      void $
        C.forkManagedTWithGracefulShutdown
          "processScheduledTriggers"
          logger
          (C.ThreadShutdown scheduledEventsGracefulShutdownAction)
          $ processScheduledTriggers
            env
            logger
            _scHttpManager
            (getSchemaCache cacheRef)
            lockedEventsCtx

instance (Monad m) => Tracing.HasReporter (PGMetadataStorageAppT m)

instance (Monad m) => HasResourceLimits (PGMetadataStorageAppT m) where
  askHTTPHandlerLimit = pure $ ResourceLimits id
  askGraphqlOperationLimit _ = pure $ \_ _ -> ResourceLimits id

instance (MonadIO m) => HttpLog (PGMetadataStorageAppT m) where
  type ExtraHttpLogMetadata (PGMetadataStorageAppT m) = ()

  emptyExtraHttpLogMetadata = ()

  buildExtraHttpLogMetadata _ = ()

  logHttpError logger loggingSettings userInfoM reqId waiReq req qErr headers =
    unLogger logger $
      mkHttpLog $
        mkHttpErrorLogContext userInfoM loggingSettings reqId waiReq req qErr Nothing Nothing headers

  logHttpSuccess logger loggingSettings userInfoM reqId waiReq reqBody response compressedResponse qTime cType headers (CommonHttpLogMetadata rb batchQueryOpLogs, ()) =
    unLogger logger $
      mkHttpLog $
        mkHttpAccessLogContext userInfoM loggingSettings reqId waiReq reqBody (BL.length response) compressedResponse qTime cType headers rb batchQueryOpLogs

instance (Monad m) => MonadExecuteQuery (PGMetadataStorageAppT m) where
  cacheLookup _ _ _ _ = pure ([], Nothing)
  cacheStore _ _ _ = pure (Right CacheStoreSkipped)

instance (MonadIO m, MonadBaseControl IO m) => UserAuthentication (Tracing.TraceT (PGMetadataStorageAppT m)) where
  resolveUserInfo logger manager headers authMode reqs =
    runExceptT $ getUserInfoWithExpTime logger manager headers authMode reqs

accessDeniedErrMsg :: Text
accessDeniedErrMsg =
  "restricted access : admin only"

instance (Monad m) => MonadMetadataApiAuthorization (PGMetadataStorageAppT m) where
  authorizeV1QueryApi query handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (requiresAdmin query && currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

  authorizeV1MetadataApi _ handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

  authorizeV2QueryApi _ handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

instance (Monad m) => ConsoleRenderer (PGMetadataStorageAppT m) where
  renderConsole path authMode enableTelemetry consoleAssetsDir consoleSentryDsn =
    return $ mkConsoleHTML path authMode enableTelemetry consoleAssetsDir consoleSentryDsn

instance (Monad m) => MonadGQLExecutionCheck (PGMetadataStorageAppT m) where
  checkGQLExecution userInfo _ enableAL sc query _ = runExceptT $ do
    req <- toParsed query
    checkQueryInAllowlist enableAL AllowlistModeGlobalOnly userInfo req sc
    return req

  executeIntrospection _ introspectionQuery _ =
    pure $ Right $ ExecStepRaw introspectionQuery

  checkGQLBatchedReqs _ _ _ _ = runExceptT $ pure ()

instance (MonadIO m, MonadBaseControl IO m) => MonadConfigApiHandler (PGMetadataStorageAppT m) where
  runConfigApiHandler = configApiGetHandler

instance (MonadIO m) => MonadQueryLog (PGMetadataStorageAppT m) where
  logQueryLog logger = unLogger logger

instance (MonadIO m) => WS.MonadWSLog (PGMetadataStorageAppT m) where
  logWSLog logger = unLogger logger

instance (Monad m) => MonadResolveSource (PGMetadataStorageAppT m) where
  getPGSourceResolver = mkPgSourceResolver <$> asks snd
  getMSSQLSourceResolver = return mkMSSQLSourceResolver

instance (Monad m) => EB.MonadQueryTags (PGMetadataStorageAppT m) where
  createQueryTags _attributes _qtSourceConfig = return $ emptyQueryTagsComment

instance (Monad m) => MonadEventLogCleanup (PGMetadataStorageAppT m) where
  runLogCleaner _ = pure $ throw400 NotSupported "Event log cleanup feature is enterprise edition only"
  generateCleanupSchedules _ _ _ = pure $ Right ()

runInSeparateTx ::
  (MonadIO m) =>
  PG.TxE QErr a ->
  MetadataStorageT (PGMetadataStorageAppT m) a
runInSeparateTx tx = do
  pool <- lift $ asks fst
  liftEitherM $ liftIO $ runExceptT $ PG.runTx pool (PG.RepeatableRead, Nothing) tx

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
  mkCatalogState . PG.getRow
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

setCatalogStateTx :: CatalogStateType -> A.Value -> PG.TxE QErr ()
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

-- | Each of the function in the type class is executed in a totally separate transaction.
--
-- To learn more about why the instance is derived as following, see Note [Generic MetadataStorageT transformer]
instance {-# OVERLAPPING #-} MonadIO m => MonadMetadataStorage (MetadataStorageT (PGMetadataStorageAppT m)) where
  fetchMetadataResourceVersion = runInSeparateTx fetchMetadataResourceVersionFromCatalog
  fetchMetadata = runInSeparateTx fetchMetadataAndResourceVersionFromCatalog
  fetchMetadataNotifications a b = runInSeparateTx $ fetchMetadataNotificationsFromCatalog a b
  setMetadata r = runInSeparateTx . setMetadataInCatalog r
  notifySchemaCacheSync a b c = runInSeparateTx $ notifySchemaCacheSyncTx a b c
  getCatalogState = runInSeparateTx getCatalogStateTx
  setCatalogState a b = runInSeparateTx $ setCatalogStateTx a b

  getMetadataDbUid = runInSeparateTx getDbId
  checkMetadataStorageHealth = runInSeparateTx $ checkDbConnection

  getDeprivedCronTriggerStats = runInSeparateTx . getDeprivedCronTriggerStatsTx
  getScheduledEventsForDelivery = runInSeparateTx getScheduledEventsForDeliveryTx
  insertCronEvents = runInSeparateTx . insertCronEventsTx
  insertOneOffScheduledEvent = runInSeparateTx . insertOneOffScheduledEventTx
  insertScheduledEventInvocation a b = runInSeparateTx $ insertInvocationTx a b
  setScheduledEventOp a b c = runInSeparateTx $ setScheduledEventOpTx a b c
  unlockScheduledEvents a b = runInSeparateTx $ unlockScheduledEventsTx a b
  unlockAllLockedScheduledEvents = runInSeparateTx unlockAllLockedScheduledEventsTx
  clearFutureCronEvents = runInSeparateTx . dropFutureCronEventsTx
  getOneOffScheduledEvents a b c = runInSeparateTx $ getOneOffScheduledEventsTx a b c
  getCronEvents a b c d = runInSeparateTx $ getCronEventsTx a b c d
  getInvocations a = runInSeparateTx $ getInvocationsTx a
  deleteScheduledEvent a b = runInSeparateTx $ deleteScheduledEventTx a b

  insertAction a b c d = runInSeparateTx $ insertActionTx a b c d
  fetchUndeliveredActionEvents = runInSeparateTx fetchUndeliveredActionEventsTx
  setActionStatus a b = runInSeparateTx $ setActionStatusTx a b
  fetchActionResponse = runInSeparateTx . fetchActionResponseTx
  clearActionData = runInSeparateTx . clearActionDataTx
  setProcessingActionLogsToPending = runInSeparateTx . setProcessingActionLogsToPendingTx

instance MonadMetadataStorageQueryAPI (MetadataStorageT (PGMetadataStorageAppT CacheBuild))

--- helper functions ---

mkConsoleHTML :: Text -> AuthMode -> Bool -> Maybe Text -> Maybe Text -> Either String Text
mkConsoleHTML path authMode enableTelemetry consoleAssetsDir consoleSentryDsn =
  renderHtmlTemplate consoleTmplt $
    -- variables required to render the template
    A.object
      [ "isAdminSecretSet" A..= isAdminSecretSet authMode,
        "consolePath" A..= consolePath,
        "enableTelemetry" A..= boolToText enableTelemetry,
        "cdnAssets" A..= boolToText (isNothing consoleAssetsDir),
        "consoleSentryDsn" A..= fromMaybe "" consoleSentryDsn,
        "assetsVersion" A..= consoleAssetsVersion,
        "serverVersion" A..= currentVersion,
        "consoleSentryDsn" A..= ("" :: Text)
      ]
  where
    consolePath = case path of
      "" -> "/console"
      r -> "/console/" <> r

    consoleTmplt = $(makeRelativeToProject "src-rsr/console.html" >>= M.embedSingleTemplate)

telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
    <> "usage stats which allows us to keep improving Hasura at warp speed. "
    <> "To read more or opt-out, visit https://hasura.io/docs/latest/graphql/core/guides/telemetry.html"

mkPgSourceResolver :: PG.PGLogger -> SourceResolver ('Postgres 'Vanilla)
mkPgSourceResolver pgLogger _ config = runExceptT do
  env <- lift Env.getEnvironment
  let PostgresSourceConnInfo urlConf poolSettings allowPrepare isoLevel _ = _pccConnectionInfo config
  -- If the user does not provide values for the pool settings, then use the default values
  let (maxConns, idleTimeout, retries) = getDefaultPGPoolSettingIfNotExists poolSettings defaultPostgresPoolSettings
  urlText <- resolveUrlConf env urlConf
  let connInfo = PG.ConnInfo retries $ PG.CDDatabaseURI $ txtToBs urlText
      connParams =
        PG.defaultConnParams
          { PG.cpIdleTime = idleTimeout,
            PG.cpConns = maxConns,
            PG.cpAllowPrepare = allowPrepare,
            PG.cpMbLifetime = _ppsConnectionLifetime =<< poolSettings,
            PG.cpTimeout = _ppsPoolTimeout =<< poolSettings
          }
  pgPool <- liftIO $ Q.initPGPool connInfo connParams pgLogger
  let pgExecCtx = mkPGExecCtx isoLevel pgPool NeverResizePool
  pure $ PGSourceConfig pgExecCtx connInfo Nothing mempty $ _pccExtensionsSchema config

mkMSSQLSourceResolver :: SourceResolver ('MSSQL)
mkMSSQLSourceResolver _name (MSSQLConnConfiguration connInfo _) = runExceptT do
  env <- lift Env.getEnvironment
  let MSSQLConnectionInfo iConnString MSSQLPoolSettings {..} = connInfo
      connOptions =
        MSPool.ConnectionOptions
          { _coConnections = fromMaybe defaultMSSQLMaxConnections _mpsMaxConnections,
            _coStripes = 1,
            _coIdleTime = _mpsIdleTimeout
          }
  (connString, mssqlPool) <- createMSSQLPool iConnString connOptions env
  let mssqlExecCtx = mkMSSQLExecCtx mssqlPool NeverResizePool
  pure $ MSSQLSourceConfig connString mssqlExecCtx
