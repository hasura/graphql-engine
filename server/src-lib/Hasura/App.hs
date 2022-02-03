{-# LANGUAGE UndecidableInstances #-}

module Hasura.App
  ( ExitCode (DatabaseMigrationError, DowngradeProcessError, MetadataCleanError, MetadataExportError, SchemaCacheInitError),
    ExitException (ExitException),
    GlobalCtx (GlobalCtx, _gcDefaultPostgresConnInfo, _gcHttpManager, _gcMetadataDbConnInfo),
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
    printErrExit,
    printErrJExit,
    printJSON,
    printYaml,
    readTlsAllowlist,
    resolvePostgresConnInfo,
    runHGEServer,
    setCatalogStateTx,
    shutdownGracefully,

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
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Environment qualified as Env
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict qualified as HM
import Data.IORef (readIORef)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import Data.Yaml qualified as Y
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
import Hasura.GraphQL.Execute.LiveQuery.Poll qualified as EL
import Hasura.GraphQL.Logging (MonadQueryLog (..))
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
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Catalog
import Hasura.RQL.Types
import Hasura.RQL.Types.Eventing.Backend
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.API.Query (requiresAdmin)
import Hasura.Server.App
import Hasura.Server.Auth
import Hasura.Server.CheckUpdates (checkForUpdates)
import Hasura.Server.Init
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Migrate (migrateCatalog)
import Hasura.Server.SchemaUpdate
import Hasura.Server.Telemetry
import Hasura.Server.Types
import Hasura.Server.Version
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.DynamicTlsPermissions (mkHttpManager)
import Network.HTTP.Client.Manager (HasHttpManagerM (..))
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
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

printErrExit :: (MonadIO m) => forall a. ExitCode -> String -> m a
printErrExit reason = liftIO . throwIO . ExitException reason . BC.pack

printErrJExit :: (A.ToJSON a, MonadIO m) => forall b. ExitCode -> a -> m b
printErrJExit reason = liftIO . throwIO . ExitException reason . BLC.toStrict . A.encode

parseHGECommand :: EnabledLogTypes impl => Parser (RawHGECommand impl)
parseHGECommand =
  subparser
    ( command
        "serve"
        ( info
            (helper <*> (HCServe <$> serveOptionsParser))
            ( progDesc "Start the GraphQL Engine Server"
                <> footerDoc (Just serveCmdFooter)
            )
        )
        <> command
          "export"
          ( info
              (pure HCExport)
              (progDesc "Export graphql-engine's metadata to stdout")
          )
        <> command
          "clean"
          ( info
              (pure HCClean)
              (progDesc "Clean graphql-engine's metadata to start afresh")
          )
        <> command
          "downgrade"
          ( info
              (HCDowngrade <$> downgradeOptionsParser)
              (progDesc "Downgrade the GraphQL Engine schema to the specified version")
          )
        <> command
          "version"
          ( info
              (pure HCVersion)
              (progDesc "Prints the version of GraphQL Engine")
          )
    )

parseArgs :: EnabledLogTypes impl => IO (HGEOptions impl)
parseArgs = do
  rawHGEOpts <- execParser opts
  env <- getEnvironment
  let eitherOpts = runWithEnv env $ mkHGEOptions rawHGEOpts
  onLeft eitherOpts $ printErrExit InvalidEnvironmentVariableOptionsError
  where
    opts =
      info
        (helper <*> hgeOpts)
        ( fullDesc
            <> header "Hasura GraphQL Engine: Realtime GraphQL API over Postgres with access control"
            <> footerDoc (Just mainCmdFooter)
        )
    hgeOpts =
      HGEOptionsG <$> parsePostgresConnInfo
        <*> parseMetadataDbUrl
        <*> parseHGECommand

printJSON :: (A.ToJSON a, MonadIO m) => a -> m ()
printJSON = liftIO . BLC.putStrLn . A.encode

printYaml :: (A.ToJSON a, MonadIO m) => a -> m ()
printYaml = liftIO . BC.putStrLn . Y.encode

mkPGLogger :: Logger Hasura -> Q.PGLogger
mkPGLogger (Logger logger) (Q.PLERetryMsg msg) =
  logger $ PGLog LevelWarn msg

-- | Context required for all graphql-engine CLI commands
data GlobalCtx = GlobalCtx
  { _gcHttpManager :: !HTTP.Manager,
    _gcMetadataDbConnInfo :: !Q.ConnInfo,
    -- | --database-url option, @'UrlConf' is required to construct default source configuration
    -- and optional retries
    _gcDefaultPostgresConnInfo :: !(Maybe (UrlConf, Q.ConnInfo), Maybe Int)
  }

readTlsAllowlist :: SchemaCacheRef -> IO [TlsAllow]
readTlsAllowlist scRef = do
  (rbsc, _) <- readIORef (_scrCache scRef)
  pure $ scTlsAllowlist $ lastBuiltSchemaCache rbsc

initGlobalCtx ::
  (MonadIO m) =>
  Env.Environment ->
  -- | the metadata DB URL
  Maybe String ->
  -- | the user's DB URL
  PostgresConnInfo (Maybe UrlConf) ->
  m GlobalCtx
initGlobalCtx env metadataDbUrl defaultPgConnInfo = do
  httpManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings

  let PostgresConnInfo dbUrlConf maybeRetries = defaultPgConnInfo
      mkConnInfoFromSource dbUrl = do
        resolvePostgresConnInfo env dbUrl maybeRetries

      mkConnInfoFromMDb mdbUrl =
        let retries = fromMaybe 1 maybeRetries
         in (Q.ConnInfo retries . Q.CDDatabaseURI . txtToBs . T.pack) mdbUrl

      mkGlobalCtx mdbConnInfo sourceConnInfo =
        pure $ GlobalCtx httpManager mdbConnInfo (sourceConnInfo, maybeRetries)

  case (metadataDbUrl, dbUrlConf) of
    (Nothing, Nothing) ->
      printErrExit
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
    _scMetadataDbPool :: !Q.PGPool,
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
    _lsPgLogger :: !Q.PGLogger
  }

-- | An application with Postgres database as a metadata storage
newtype PGMetadataStorageAppT m a = PGMetadataStorageAppT {runPGMetadataStorageAppT :: (Q.PGPool, Q.PGLogger) -> m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadMask,
      HasHttpManagerM,
      HasServerConfigCtx,
      MonadReader (Q.PGPool, Q.PGLogger)
    )
    via (ReaderT (Q.PGPool, Q.PGLogger) m)

deriving via
  (ReaderT (Q.PGPool, Q.PGLogger) m)
  instance
    MonadBase IO m => MonadBase IO (PGMetadataStorageAppT m)

deriving via
  (ReaderT (Q.PGPool, Q.PGLogger) m)
  instance
    MonadBaseControl IO m => MonadBaseControl IO (PGMetadataStorageAppT m)

deriving via
  (ReaderT (Q.PGPool, Q.PGLogger))
  instance
    MonadTrans PGMetadataStorageAppT

resolvePostgresConnInfo ::
  (MonadIO m) => Env.Environment -> UrlConf -> Maybe Int -> m Q.ConnInfo
resolvePostgresConnInfo env dbUrlConf maybeRetries = do
  dbUrlText <-
    runExcept (resolveUrlConf env dbUrlConf) `onLeft` \err ->
      liftIO (printErrExit InvalidDatabaseConnectionParamsError (BLC.unpack $ A.encode err))
  pure $ Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs dbUrlText
  where
    retries = fromMaybe 1 maybeRetries

-- | Initializes or migrates the catalog and returns the context required to start the server.
initialiseServeCtx ::
  (C.ForkableMonadIO m, MonadCatch m) =>
  Env.Environment ->
  GlobalCtx ->
  ServeOptions Hasura ->
  ManagedT m ServeCtx
initialiseServeCtx env GlobalCtx {..} so@ServeOptions {..} = do
  instanceId <- liftIO generateInstanceId
  latch <- liftIO newShutdownLatch
  loggers@(Loggers loggerCtx logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel
  -- log serve options
  unLogger logger $ serveOptsToLog so

  -- log postgres connection info
  unLogger logger $ connInfoToLog _gcMetadataDbConnInfo

  metadataDbPool <- liftIO $ Q.initPGPool _gcMetadataDbConnInfo soConnParams pgLogger

  let maybeDefaultSourceConfig =
        fst _gcDefaultPostgresConnInfo <&> \(dbUrlConf, _) ->
          let connSettings =
                PostgresPoolSettings
                  { _ppsMaxConnections = Just $ Q.cpConns soConnParams,
                    _ppsIdleTimeout = Just $ Q.cpIdleTime soConnParams,
                    _ppsRetries = snd _gcDefaultPostgresConnInfo <|> Just 1,
                    _ppsPoolTimeout = Q.cpTimeout soConnParams,
                    _ppsConnectionLifetime = Q.cpMbLifetime soConnParams
                  }
              sourceConnInfo = PostgresSourceConnInfo dbUrlConf (Just connSettings) (Q.cpAllowPrepare soConnParams) soTxIso Nothing
           in PostgresConnConfiguration sourceConnInfo Nothing
      sqlGenCtx = SQLGenCtx soStringifyNum soDangerousBooleanCollapse soOptimizePermissionFilters

  let serverConfigCtx =
        ServerConfigCtx
          soInferFunctionPermissions
          soEnableRemoteSchemaPermissions
          sqlGenCtx
          soEnableMaintenanceMode
          soExperimentalFeatures
          soEventingMode
          soReadOnlyMode

  (rebuildableSchemaCache, _) <-
    lift . flip onException (flushLogger loggerCtx) $
      migrateCatalogSchema
        env
        logger
        metadataDbPool
        maybeDefaultSourceConfig
        _gcHttpManager
        serverConfigCtx
        (mkPgSourceResolver pgLogger)
        mkMSSQLSourceResolver

  -- Start a background thread for listening schema sync events from other server instances,
  metaVersionRef <- liftIO $ STM.newEmptyTMVarIO

  -- An interval of 0 indicates that no schema sync is required
  case soSchemaPollInterval of
    Skip -> unLogger logger $ mkGenericStrLog LevelInfo "schema-sync" "Schema sync disabled"
    Interval i -> do
      unLogger logger $ mkGenericStrLog LevelInfo "schema-sync" ("Schema sync enabled. Polling at " <> show i)
      void $ startSchemaSyncListenerThread logger metadataDbPool instanceId i metaVersionRef

  schemaCacheRef <- initialiseCache rebuildableSchemaCache

  srvMgr <- liftIO $ mkHttpManager (readTlsAllowlist schemaCacheRef)

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
  Q.PGPool ->
  Maybe (SourceConnConfiguration ('Postgres 'Vanilla)) ->
  HTTP.Manager ->
  ServerConfigCtx ->
  SourceResolver ('Postgres 'Vanilla) ->
  SourceResolver ('MSSQL) ->
  m (RebuildableSchemaCache, UTCTime)
migrateCatalogSchema
  env
  logger
  pool
  defaultSourceConfig
  httpManager
  serverConfigCtx
  pgSourceResolver
  mssqlSourceResolver = do
    currentTime <- liftIO Clock.getCurrentTime
    initialiseResult <- runExceptT $ do
      -- TODO: should we allow the migration to happen during maintenance mode?
      -- Allowing this can be a sanity check, to see if the hdb_catalog in the
      -- DB has been set correctly
      (migrationResult, metadata) <-
        Q.runTx pool (Q.Serializable, Just Q.ReadWrite) $
          migrateCatalog
            defaultSourceConfig
            (_sccMaintenanceMode serverConfigCtx)
            currentTime
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
        liftIO (printErrExit DatabaseMigrationError (BLC.unpack $ A.encode err))
    unLogger logger migrationResult
    pure (schemaCache, currentTime)

-- | Run a transaction and if an error is encountered, log the error and abort the program
runTxIO :: Q.PGPool -> Q.TxMode -> Q.TxE QErr a -> IO a
runTxIO pool isoLevel tx = do
  eVal <- liftIO $ runExceptT $ Q.runTx pool isoLevel tx
  onLeft eVal (printErrJExit DatabaseMigrationError)

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
waitForShutdown = C.takeMVar . unShutdownLatch

-- | Initiate a graceful shutdown of the server associated with the provided
-- latch.
shutdownGracefully :: ShutdownLatch -> IO ()
shutdownGracefully = void . flip C.tryPutMVar () . unShutdownLatch

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
    EB.MonadQueryTags m
  ) =>
  (ServerCtx -> Spock.SpockT m ()) ->
  Env.Environment ->
  ServeOptions impl ->
  ServeCtx ->
  -- and mutations

  -- | start time
  UTCTime ->
  Maybe EL.LiveQueryPostPollHook ->
  ServerMetrics ->
  EKG.Store EKG.EmptyMetrics ->
  ManagedT m ()
runHGEServer setupHook env serveOptions serveCtx initTime postPollHook serverMetrics ekgStore = do
  waiApplication <-
    mkHGEServer setupHook env serveOptions serveCtx initTime postPollHook serverMetrics ekgStore

  let warpSettings :: Warp.Settings
      warpSettings =
        Warp.setPort (soPort serveOptions)
          . Warp.setHost (soHost serveOptions)
          . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
          . Warp.setInstallShutdownHandler shutdownHandler
          . setForkIOWithMetrics
          $ Warp.defaultSettings

      setForkIOWithMetrics :: Warp.Settings -> Warp.Settings
      setForkIOWithMetrics = Warp.setFork \f -> do
        void $
          C.forkIOWithUnmask
            ( \unmask ->
                bracket_
                  (EKG.Gauge.inc $ smWarpThreads serverMetrics)
                  (EKG.Gauge.dec $ smWarpThreads serverMetrics)
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
    EB.MonadQueryTags m
  ) =>
  (ServerCtx -> Spock.SpockT m ()) ->
  Env.Environment ->
  ServeOptions impl ->
  ServeCtx ->
  -- and mutations

  -- | start time
  UTCTime ->
  Maybe EL.LiveQueryPostPollHook ->
  ServerMetrics ->
  EKG.Store EKG.EmptyMetrics ->
  ManagedT m Application
mkHGEServer setupHook env ServeOptions {..} ServeCtx {..} initTime postPollHook serverMetrics ekgStore = do
  -- Comment this to enable expensive assertions from "GHC.AssertNF". These
  -- will log lines to STDOUT containing "not in normal form". In the future we
  -- could try to integrate this into our tests. For now this is a development
  -- tool.
  --
  -- NOTE: be sure to compile WITHOUT code coverage, for this to work properly.
  liftIO disableAssertNF

  let sqlGenCtx = SQLGenCtx soStringifyNum soDangerousBooleanCollapse soOptimizePermissionFilters
      Loggers loggerCtx logger _ = _scLoggers
  --SchemaSyncCtx{..} = _scSchemaSyncCtx

  authModeRes <-
    runExceptT $
      setupAuthMode
        soAdminSecret
        soAuthHook
        soJwtSecret
        soUnAuthRole
        _scHttpManager
        logger

  authMode <- onLeft authModeRes (printErrExit AuthConfigurationError . T.unpack)

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
          soEnableTelemetry
          _scInstanceId
          soEnabledAPIs
          soLiveQueryOpts
          soResponseInternalErrorsConfig
          postPollHook
          _scSchemaCacheRef
          ekgStore
          serverMetrics
          soEnableRemoteSchemaPermissions
          soInferFunctionPermissions
          soConnectionOptions
          soWebsocketKeepAlive
          soEnableMaintenanceMode
          soEventingMode
          soReadOnlyMode
          soExperimentalFeatures
          _scEnabledLogTypes
          soWebsocketConnectionInitTimeout

  let serverConfigCtx =
        ServerConfigCtx
          soInferFunctionPermissions
          soEnableRemoteSchemaPermissions
          sqlGenCtx
          soEnableMaintenanceMode
          soExperimentalFeatures
          soEventingMode
          soReadOnlyMode

  -- Log Warning if deprecated environment variables are used
  sources <- scSources <$> liftIO (getSCFromRef cacheRef)
  liftIO $ logDeprecatedEnvVars logger env sources

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSCFromRef cacheRef)
  liftIO $ logInconsObjs logger inconsObjs

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

  let eventLogBehavior =
        LogBehavior
          { _lbHeader = if soLogHeadersFromEnv then LogEnvValue else LogEnvVarname,
            _lbResponse = if soDevMode then LogEntireResponse else LogSanitisedResponse
          }

  lockedEventsCtx <-
    liftIO $
      LockedEventsCtx
        <$> STM.newTVarIO mempty
        <*> STM.newTVarIO mempty
        <*> STM.newTVarIO mempty
        <*> STM.newTVarIO mempty

  case soEventingMode of
    EventingEnabled -> do
      startEventTriggerPollerThread logger eventLogBehavior lockedEventsCtx cacheRef
      startAsyncActionsPollerThread logger lockedEventsCtx cacheRef actionSubState

      -- start a background thread to create new cron events
      _cronEventsThread <-
        C.forkManagedT "runCronEventsGenerator" logger $
          runCronEventsGenerator logger (getSCFromRef cacheRef)

      startScheduledEventsPollerThread logger eventLogBehavior lockedEventsCtx cacheRef
    EventingDisabled ->
      unLogger logger $ mkGenericStrLog LevelInfo "server" "starting in eventing disabled mode"

  -- start a background thread to check for updates
  _updateThread <-
    C.forkManagedT "checkForUpdates" logger $
      liftIO $ checkForUpdates loggerCtx _scHttpManager

  -- start a background thread for telemetry
  dbUidE <- runMetadataStorageT getDatabaseUid
  _telemetryThread <-
    if soEnableTelemetry
      then do
        lift . unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice

        (dbId, pgVersion) <-
          liftIO $
            runTxIO _scMetadataDbPool (Q.ReadCommitted, Nothing) $
              (,) <$> liftEither dbUidE <*> getPgVersion

        telemetryThread <-
          C.forkManagedT "runTelemetry" logger $
            liftIO $ runTelemetry logger _scHttpManager (getSCFromRef cacheRef) dbId _scInstanceId pgVersion
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
    prepareScheduledEvents (Logger logger) = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "scheduled_triggers" "preparing data"
      res <- runMetadataStorageT unlockAllLockedScheduledEvents
      onLeft res $ printErrJExit EventSubSystemError

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
          let sourceNameString = T.unpack $ sourceNameToText sourceName
          logger $ mkGenericStrLog LevelInfo "event_triggers" $ "unlocking events of source: " ++ sourceNameString
          onJust (HM.lookup sourceName lockedEvents) $ \sourceLockedEvents -> do
            unlockEventsInSource @b sourceConfig sourceLockedEvents >>= \case
              Left err ->
                logger $
                  mkGenericStrLog LevelWarn "event_trigger" $
                    "Error while unlocking event trigger events of source: " ++ sourceNameString ++ " error:" ++ show err
              Right count ->
                logger $
                  mkGenericStrLog LevelInfo "event_trigger" $
                    show count ++ " events of source " ++ sourceNameString ++ " were successfully unlocked"

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
                  mkGenericStrLog LevelWarn (T.pack actionType) $
                    "Error while unlocking the processing  "
                      <> show actionType
                      <> " err - "
                      <> show err
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

    startEventTriggerPollerThread logger eventLogBehavior lockedEventsCtx cacheRef = do
      let maxEvThrds = fromMaybe defaultMaxEventThreads soEventsHttpPoolSize
          fetchI = milliseconds $ fromMaybe (Milliseconds defaultFetchInterval) soEventsFetchInterval
          allSources = HM.elems $ scSources $ lastBuiltSchemaCache _scSchemaCache

      unless (getNonNegativeInt soEventsFetchBatchSize == 0 || soEventsFetchInterval == Just 0) $ do
        -- Don't start the events poller thread when fetchBatchSize or fetchInterval is 0
        -- prepare event triggers data
        eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEvThrds fetchI soEventsFetchBatchSize
        let eventsGracefulShutdownAction =
              waitForProcessingAction
                logger
                "event_triggers"
                (length <$> readTVarIO (leEvents lockedEventsCtx))
                (EventTriggerShutdownAction (shutdownEventTriggerEvents allSources logger lockedEventsCtx))
                soGracefulShutdownTimeout
        unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"
        void $
          C.forkManagedTWithGracefulShutdown
            "processEventQueue"
            logger
            (C.ThreadShutdown (liftIO eventsGracefulShutdownAction))
            $ processEventQueue
              logger
              eventLogBehavior
              _scHttpManager
              (getSCFromRef cacheRef)
              eventEngineCtx
              lockedEventsCtx
              serverMetrics
              soEnableMaintenanceMode

    startAsyncActionsPollerThread logger lockedEventsCtx cacheRef actionSubState = do
      -- start a background thread to handle async actions
      case soAsyncActionsFetchInterval of
        Skip -> pure () -- Don't start the poller thread
        Interval sleepTime -> do
          let label = "asyncActionsProcessor"
              asyncActionGracefulShutdownAction =
                ( liftWithStateless \lowerIO ->
                    ( waitForProcessingAction
                        logger
                        "async_actions"
                        (length <$> readTVarIO (leActionEvents lockedEventsCtx))
                        (MetadataDBShutdownAction (hoist lowerIO (shutdownAsyncActions lockedEventsCtx)))
                        soGracefulShutdownTimeout
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
                (_scrCache cacheRef)
                (leActionEvents lockedEventsCtx)
                _scHttpManager
                sleepTime
                Nothing

      -- start a background thread to handle async action live queries
      void $
        C.forkManagedT "asyncActionSubscriptionsProcessor" logger $
          asyncActionSubscriptionsProcessor actionSubState

    startScheduledEventsPollerThread logger eventLogBehavior lockedEventsCtx cacheRef = do
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
                    soGracefulShutdownTimeout
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
            eventLogBehavior
            _scHttpManager
            (getSCFromRef cacheRef)
            lockedEventsCtx

instance (Monad m) => Tracing.HasReporter (PGMetadataStorageAppT m)

instance (Monad m) => HasResourceLimits (PGMetadataStorageAppT m) where
  askHTTPHandlerLimit = pure $ ResourceLimits id
  askGraphqlOperationLimit = pure $ \_ _ -> ResourceLimits id

instance (MonadIO m) => HttpLog (PGMetadataStorageAppT m) where
  type ExtraHttpLogMetadata (PGMetadataStorageAppT m) = ()

  emptyExtraHttpLogMetadata = ()

  buildExtraHttpLogMetadata _ = ()

  logHttpError logger enabledLogTypes userInfoM reqId waiReq req qErr headers =
    unLogger logger $
      mkHttpLog $
        mkHttpErrorLogContext userInfoM enabledLogTypes reqId waiReq req qErr Nothing Nothing headers

  logHttpSuccess logger enabledLogTypes userInfoM reqId waiReq reqBody _response compressedResponse qTime cType headers (CommonHttpLogMetadata rb batchQueryOpLogs, ()) =
    unLogger logger $
      mkHttpLog $
        mkHttpAccessLogContext userInfoM enabledLogTypes reqId waiReq reqBody compressedResponse qTime cType headers rb batchQueryOpLogs

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
  renderConsole path authMode enableTelemetry consoleAssetsDir =
    return $ mkConsoleHTML path authMode enableTelemetry consoleAssetsDir

instance (Monad m) => MonadGQLExecutionCheck (PGMetadataStorageAppT m) where
  checkGQLExecution userInfo _ enableAL sc query = runExceptT $ do
    req <- toParsed query
    checkQueryInAllowlist enableAL userInfo req sc
    return req

  executeIntrospection _ introspectionQuery _ =
    pure $ Right $ ExecStepRaw introspectionQuery

instance (MonadIO m, MonadBaseControl IO m) => MonadConfigApiHandler (PGMetadataStorageAppT m) where
  runConfigApiHandler = configApiGetHandler

instance (MonadIO m) => MonadQueryLog (PGMetadataStorageAppT m) where
  logQueryLog = unLogger

instance (MonadIO m) => WS.MonadWSLog (PGMetadataStorageAppT m) where
  logWSLog = unLogger

instance (Monad m) => MonadResolveSource (PGMetadataStorageAppT m) where
  getPGSourceResolver = mkPgSourceResolver <$> asks snd
  getMSSQLSourceResolver = return mkMSSQLSourceResolver

instance (Monad m) => EB.MonadQueryTags (PGMetadataStorageAppT m) where
  createQueryTags _attributes _qtSourceConfig = return $ emptyQueryTagsComment

runInSeparateTx ::
  (MonadIO m) =>
  Q.TxE QErr a ->
  MetadataStorageT (PGMetadataStorageAppT m) a
runInSeparateTx tx = do
  pool <- lift $ asks fst
  liftEitherM $ liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Nothing) tx

notifySchemaCacheSyncTx :: MetadataResourceVersion -> InstanceId -> CacheInvalidations -> Q.TxE QErr ()
notifySchemaCacheSyncTx (MetadataResourceVersion resourceVersion) instanceId invalidations = do
  Q.Discard () <-
    Q.withQE
      defaultTxErrorHandler
      [Q.sql|
      INSERT INTO hdb_catalog.hdb_schema_notifications(id, notification, resource_version, instance_id)
      VALUES (1, $1::json, $2, $3::uuid)
      ON CONFLICT (id) DO UPDATE SET
        notification = $1::json,
        resource_version = $2,
        instance_id = $3::uuid
    |]
      (Q.AltJ invalidations, resourceVersion, instanceId)
      True
  pure ()

getCatalogStateTx :: Q.TxE QErr CatalogState
getCatalogStateTx =
  mkCatalogState . Q.getRow
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
    SELECT hasura_uuid::text, cli_state::json, console_state::json
      FROM hdb_catalog.hdb_version
  |]
      ()
      False
  where
    mkCatalogState (dbId, Q.AltJ cliState, Q.AltJ consoleState) =
      CatalogState dbId cliState consoleState

setCatalogStateTx :: CatalogStateType -> A.Value -> Q.TxE QErr ()
setCatalogStateTx stateTy stateValue =
  case stateTy of
    CSTCli ->
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
        UPDATE hdb_catalog.hdb_version
           SET cli_state = $1
      |]
        (Identity $ Q.AltJ stateValue)
        False
    CSTConsole ->
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
        UPDATE hdb_catalog.hdb_version
           SET console_state = $1
      |]
        (Identity $ Q.AltJ stateValue)
        False

-- | Each of the function in the type class is executed in a totally separate transaction.
--
-- To learn more about why the instance is derived as following, see Note [Generic MetadataStorageT transformer]
instance {-# OVERLAPPING #-} MonadIO m => MonadMetadataStorage (MetadataStorageT (PGMetadataStorageAppT m)) where
  fetchMetadataResourceVersion = runInSeparateTx fetchMetadataResourceVersionFromCatalog
  fetchMetadata = runInSeparateTx fetchMetadataAndResourceVersionFromCatalog
  fetchMetadataNotifications a b = runInSeparateTx $ fetchMetadataNotificationsFromCatalog a b
  setMetadata r = runInSeparateTx . setMetadataInCatalog (Just r)
  notifySchemaCacheSync a b c = runInSeparateTx $ notifySchemaCacheSyncTx a b c
  getCatalogState = runInSeparateTx getCatalogStateTx
  setCatalogState a b = runInSeparateTx $ setCatalogStateTx a b

  getDatabaseUid = runInSeparateTx getDbId
  checkMetadataStorageHealth = lift (asks fst) >>= checkDbConnection

  getDeprivedCronTriggerStats = runInSeparateTx . getDeprivedCronTriggerStatsTx
  getScheduledEventsForDelivery = runInSeparateTx getScheduledEventsForDeliveryTx
  insertCronEvents = runInSeparateTx . insertCronEventsTx
  insertOneOffScheduledEvent = runInSeparateTx . insertOneOffScheduledEventTx
  insertScheduledEventInvocation a b = runInSeparateTx $ insertInvocationTx a b
  setScheduledEventOp a b c = runInSeparateTx $ setScheduledEventOpTx a b c
  unlockScheduledEvents a b = runInSeparateTx $ unlockScheduledEventsTx a b
  unlockAllLockedScheduledEvents = runInSeparateTx unlockAllLockedScheduledEventsTx
  clearFutureCronEvents = runInSeparateTx . dropFutureCronEventsTx
  getOneOffScheduledEvents a b = runInSeparateTx $ getOneOffScheduledEventsTx a b
  getCronEvents a b c = runInSeparateTx $ getCronEventsTx a b c
  getInvocations a b = runInSeparateTx $ getInvocationsTx a b
  deleteScheduledEvent a b = runInSeparateTx $ deleteScheduledEventTx a b

  insertAction a b c d = runInSeparateTx $ insertActionTx a b c d
  fetchUndeliveredActionEvents = runInSeparateTx fetchUndeliveredActionEventsTx
  setActionStatus a b = runInSeparateTx $ setActionStatusTx a b
  fetchActionResponse = runInSeparateTx . fetchActionResponseTx
  clearActionData = runInSeparateTx . clearActionDataTx
  setProcessingActionLogsToPending = runInSeparateTx . setProcessingActionLogsToPendingTx

instance MonadMetadataStorageQueryAPI (MetadataStorageT (PGMetadataStorageAppT CacheBuild))

--- helper functions ---

mkConsoleHTML :: Text -> AuthMode -> Bool -> Maybe Text -> Either String Text
mkConsoleHTML path authMode enableTelemetry consoleAssetsDir =
  renderHtmlTemplate consoleTmplt $
    -- variables required to render the template
    A.object
      [ "isAdminSecretSet" A..= isAdminSecretSet authMode,
        "consolePath" A..= consolePath,
        "enableTelemetry" A..= boolToText enableTelemetry,
        "cdnAssets" A..= boolToText (isNothing consoleAssetsDir),
        "assetsVersion" A..= consoleAssetsVersion,
        "serverVersion" A..= currentVersion
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

mkPgSourceResolver :: Q.PGLogger -> SourceResolver ('Postgres 'Vanilla)
mkPgSourceResolver pgLogger _ config = runExceptT do
  env <- lift Env.getEnvironment
  let PostgresSourceConnInfo urlConf poolSettings allowPrepare isoLevel _ = _pccConnectionInfo config
  -- If the user does not provide values for the pool settings, then use the default values
  let (maxConns, idleTimeout, retries) = getDefaultPGPoolSettingIfNotExists poolSettings defaultPostgresPoolSettings
  urlText <- resolveUrlConf env urlConf
  let connInfo = Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs urlText
      connParams =
        Q.defaultConnParams
          { Q.cpIdleTime = idleTimeout,
            Q.cpConns = maxConns,
            Q.cpAllowPrepare = allowPrepare,
            Q.cpMbLifetime = _ppsConnectionLifetime =<< poolSettings,
            Q.cpTimeout = _ppsPoolTimeout =<< poolSettings
          }
  pgPool <- liftIO $ Q.initPGPool connInfo connParams pgLogger
  let pgExecCtx = mkPGExecCtx isoLevel pgPool
  pure $ PGSourceConfig pgExecCtx connInfo Nothing mempty

mkMSSQLSourceResolver :: SourceResolver ('MSSQL)
mkMSSQLSourceResolver _name (MSSQLConnConfiguration connInfo _) = runExceptT do
  env <- lift Env.getEnvironment
  (connString, mssqlPool) <- createMSSQLPool connInfo env
  let mssqlExecCtx = mkMSSQLExecCtx mssqlPool
  pure $ MSSQLSourceConfig connString mssqlExecCtx
