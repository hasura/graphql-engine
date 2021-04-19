{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.App where

import           Hasura.Prelude

import qualified Control.Concurrent.Async.Lifted.Safe       as LA
import qualified Control.Concurrent.Extended                as C
import qualified Control.Concurrent.STM                     as STM
import qualified Control.Exception.Lifted                   as LE
import qualified Data.Aeson                                 as A
import qualified Data.ByteString.Char8                      as BC
import qualified Data.ByteString.Lazy.Char8                 as BLC
import qualified Data.Environment                           as Env
import qualified Data.HashMap.Strict                        as HM
import qualified Data.Set                                   as Set
import qualified Data.Text                                  as T
import qualified Data.Time.Clock                            as Clock
import qualified Data.Yaml                                  as Y
import qualified Database.PG.Query                          as Q
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Client.TLS                    as HTTP
import qualified Network.Wai.Handler.Warp                   as Warp
import qualified System.Log.FastLogger                      as FL
import qualified System.Metrics                             as EKG
import qualified System.Metrics.Gauge                       as EKG.Gauge
import qualified Text.Mustache.Compile                      as M
import qualified Web.Spock.Core                             as Spock

import           Control.Concurrent.STM.TVar                (TVar, readTVarIO)
import           Control.Exception                          (bracket_, throwIO)
import           Control.Monad.Catch                        (Exception, MonadCatch, MonadMask,
                                                             MonadThrow, onException)
import           Control.Monad.Morph                        (hoist)
import           Control.Monad.STM                          (atomically)
import           Control.Monad.Stateless
import           Control.Monad.Trans.Control                (MonadBaseControl (..))
import           Control.Monad.Trans.Managed                (ManagedT (..), allocate)
import           Control.Monad.Unique
import           Data.FileEmbed                             (makeRelativeToProject)
import           Data.Time.Clock                            (UTCTime)
#ifndef PROFILING
import           GHC.AssertNF
#endif
import           Network.HTTP.Client.Extended
import           Options.Applicative
import           System.Environment                         (getEnvironment)

import qualified Hasura.GraphQL.Execute.LiveQuery.Poll      as EL
import qualified Hasura.GraphQL.Transport.WebSocket.Server  as WS
import qualified Hasura.Tracing                             as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.Eventing.Common
import           Hasura.Eventing.EventTrigger
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.GraphQL.Execute                     (MonadGQLExecutionCheck (..),
                                                             checkQueryInAllowlist)
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Action.Subscription
import           Hasura.GraphQL.Logging                     (MonadQueryLog (..))
import           Hasura.GraphQL.Transport.HTTP              (MonadExecuteQuery (..))
import           Hasura.GraphQL.Transport.HTTP.Protocol     (toParsed)
import           Hasura.Logging
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Source
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.API.Query                    (requiresAdmin, runQueryM)
import           Hasura.Server.App
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates                 (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Migrate                      (getMigratedFrom, migrateCatalog)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Types
import           Hasura.Server.Version
import           Hasura.Session

data ExitCode
-- these are used during server initialization:
  = InvalidEnvironmentVariableOptionsError
  | InvalidDatabaseConnectionParamsError
  | AuthConfigurationError
  | EventSubSystemError
  | DatabaseMigrationError
  | SchemaCacheInitError -- ^ used by MT because it initialises the schema cache only
  -- these are used in app/Main.hs:
  | MetadataExportError
  | MetadataCleanError
  | ExecuteProcessError
  | DowngradeProcessError
  deriving Show

data ExitException
  = ExitException
  { eeCode    :: !ExitCode
  , eeMessage :: !BC.ByteString
  } deriving (Show)

instance Exception ExitException

printErrExit :: (MonadIO m) => forall a . ExitCode -> String -> m a
printErrExit reason = liftIO . throwIO . ExitException reason . BC.pack

printErrJExit :: (A.ToJSON a, MonadIO m) => forall b . ExitCode -> a -> m b
printErrJExit reason = liftIO . throwIO . ExitException reason . BLC.toStrict . A.encode

parseHGECommand :: EnabledLogTypes impl => Parser (RawHGECommand impl)
parseHGECommand =
  subparser
    ( command "serve" (info (helper <*> (HCServe <$> serveOptionsParser))
          ( progDesc "Start the GraphQL Engine Server"
            <> footerDoc (Just serveCmdFooter)
          ))
        <> command "export" (info (pure  HCExport)
          ( progDesc "Export graphql-engine's metadata to stdout" ))
        <> command "clean" (info (pure  HCClean)
          ( progDesc "Clean graphql-engine's metadata to start afresh" ))
        <> command "execute" (info (pure  HCExecute)
          ( progDesc "Execute a query" ))
        <> command "downgrade" (info (HCDowngrade <$> downgradeOptionsParser)
          (progDesc "Downgrade the GraphQL Engine schema to the specified version"))
        <> command "version" (info (pure  HCVersion)
          (progDesc "Prints the version of GraphQL Engine"))
    )

parseArgs :: EnabledLogTypes impl => IO (HGEOptions impl)
parseArgs = do
  rawHGEOpts <- execParser opts
  env <- getEnvironment
  let eitherOpts = runWithEnv env $ mkHGEOptions rawHGEOpts
  onLeft eitherOpts $ printErrExit InvalidEnvironmentVariableOptionsError
  where
    opts = info (helper <*> hgeOpts)
           ( fullDesc <>
             header "Hasura GraphQL Engine: Realtime GraphQL API over Postgres with access control" <>
             footerDoc (Just mainCmdFooter)
           )
    hgeOpts = HGEOptionsG <$> parsePostgresConnInfo
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
data GlobalCtx
  = GlobalCtx
  { _gcHttpManager             :: !HTTP.Manager
  , _gcMetadataDbConnInfo      :: !Q.ConnInfo
  , _gcDefaultPostgresConnInfo :: !(Maybe (UrlConf, Q.ConnInfo), Maybe Int)
    -- ^ --database-url option, @'UrlConf' is required to construct default source configuration
    -- and optional retries
  }


initGlobalCtx
  :: (MonadIO m)
  => Env.Environment
  -> Maybe String
  -- ^ the metadata DB URL
  -> PostgresConnInfo (Maybe UrlConf)
  -- ^ the user's DB URL
  -> m GlobalCtx
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
      printErrExit InvalidDatabaseConnectionParamsError
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
data ServeCtx
  = ServeCtx
  { _scHttpManager    :: !HTTP.Manager
  , _scInstanceId     :: !InstanceId
  , _scLoggers        :: !Loggers
  , _scMetadataDbPool :: !Q.PGPool
  , _scShutdownLatch  :: !ShutdownLatch
  , _scSchemaCache    :: !RebuildableSchemaCache
  , _scSchemaCacheRef :: !SchemaCacheRef
  , _scMetaVersionRef :: !(STM.TMVar MetadataResourceVersion)
  }

-- | Collection of the LoggerCtx, the regular Logger and the PGLogger
-- TODO (from master): better naming?
data Loggers
  = Loggers
  { _lsLoggerCtx :: !(LoggerCtx Hasura)
  , _lsLogger    :: !(Logger Hasura)
  , _lsPgLogger  :: !Q.PGLogger
  }

-- | An application with Postgres database as a metadata storage
newtype PGMetadataStorageApp a
  = PGMetadataStorageApp {runPGMetadataStorageApp :: (Q.PGPool, Q.PGLogger) -> IO a}
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadBase IO, MonadBaseControl IO
           , MonadCatch, MonadThrow, MonadMask
           , MonadUnique, MonadReader (Q.PGPool, Q.PGLogger)
           ) via (ReaderT (Q.PGPool, Q.PGLogger) IO)

resolvePostgresConnInfo
  :: (MonadIO m) => Env.Environment -> UrlConf -> Maybe Int -> m Q.ConnInfo
resolvePostgresConnInfo env dbUrlConf maybeRetries = do
  dbUrlText <-
    runExcept (resolveUrlConf env dbUrlConf) `onLeft` \err ->
      liftIO (printErrExit InvalidDatabaseConnectionParamsError (BLC.unpack $ A.encode err))
  pure $ Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs dbUrlText
  where
    retries = fromMaybe 1 maybeRetries

-- | Initializes or migrates the catalog and returns the context required to start the server.
initialiseServeCtx
  :: (HasVersion, C.ForkableMonadIO m, MonadCatch m)
  => Env.Environment
  -> GlobalCtx
  -> ServeOptions Hasura
  -> ManagedT m ServeCtx
initialiseServeCtx env GlobalCtx{..} so@ServeOptions{..} = do
  instanceId <- liftIO generateInstanceId
  latch <- liftIO newShutdownLatch
  loggers@(Loggers loggerCtx logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel
  -- log serve options
  unLogger logger $ serveOptsToLog so

  -- log postgres connection info
  unLogger logger $ connInfoToLog _gcMetadataDbConnInfo

  metadataDbPool <- liftIO $ Q.initPGPool _gcMetadataDbConnInfo soConnParams pgLogger

  let maybeDefaultSourceConfig = fst _gcDefaultPostgresConnInfo <&> \(dbUrlConf, _) ->
        let connSettings = PostgresPoolSettings
                           { _ppsMaxConnections = Just $ Q.cpConns soConnParams
                           , _ppsIdleTimeout    = Just $ Q.cpIdleTime soConnParams
                           , _ppsRetries        = snd _gcDefaultPostgresConnInfo <|> Just 1
                           }
            sourceConnInfo = PostgresSourceConnInfo dbUrlConf (Just connSettings) $ Q.cpAllowPrepare soConnParams
        in PostgresConnConfiguration sourceConnInfo Nothing
      sqlGenCtx = SQLGenCtx soStringifyNum soDangerousBooleanCollapse

  let serverConfigCtx =
        ServerConfigCtx soInferFunctionPermissions soEnableRemoteSchemaPermissions
                        sqlGenCtx soEnableMaintenanceMode soExperimentalFeatures

  (rebuildableSchemaCache, _) <-
    lift . flip onException (flushLogger loggerCtx) $
    migrateCatalogSchema env logger metadataDbPool maybeDefaultSourceConfig _gcHttpManager
      serverConfigCtx (mkPgSourceResolver pgLogger)


  -- Start a background thread for listening schema sync events from other server instances,
  metaVersionRef <- liftIO $ STM.newEmptyTMVarIO

  -- An interval of 0 indicates that no schema sync is required
  case soSchemaPollInterval of
    Skip -> unLogger logger $ mkGenericStrLog LevelInfo "schema-sync" "Schema sync disabled"
    Interval i -> do
      unLogger logger $ mkGenericStrLog LevelInfo "schema-sync" ("Schema sync enabled. Polling at " <> show i)
      void $ startSchemaSyncListenerThread logger metadataDbPool instanceId i metaVersionRef

  -- See Note [Temporarily disabling query plan caching]
  -- (planCache, schemaCacheRef) <- initialiseCache
  schemaCacheRef <- initialiseCache rebuildableSchemaCache

  pure $ ServeCtx _gcHttpManager instanceId loggers metadataDbPool latch
                  rebuildableSchemaCache schemaCacheRef metaVersionRef

mkLoggers
  :: (MonadIO m, MonadBaseControl IO m)
  => HashSet (EngineLogType Hasura)
  -> LogLevel
  -> ManagedT m Loggers
mkLoggers enabledLogs logLevel = do
  loggerCtx <- mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
  let logger = mkLogger loggerCtx
      pgLogger = mkPGLogger logger
  return $ Loggers loggerCtx logger pgLogger


-- | helper function to initialize or migrate the @hdb_catalog@ schema (used by pro as well)
migrateCatalogSchema
  :: (HasVersion, MonadIO m, MonadBaseControl IO m)
  => Env.Environment -> Logger Hasura -> Q.PGPool -> Maybe (SourceConnConfiguration 'Postgres)
  -> HTTP.Manager -> ServerConfigCtx
  -> SourceResolver
  -> m (RebuildableSchemaCache, UTCTime)
migrateCatalogSchema env logger pool defaultSourceConfig
                     httpManager serverConfigCtx
                     sourceResolver = do
  currentTime <- liftIO Clock.getCurrentTime
  initialiseResult <- runExceptT $ do
    -- TODO: should we allow the migration to happen during maintenance mode?
    -- Allowing this can be a sanity check, to see if the hdb_catalog in the
    -- DB has been set correctly
    (migrationResult, metadata) <-
      Q.runTx pool (Q.Serializable, Just Q.ReadWrite) $
        migrateCatalog defaultSourceConfig (_sccMaintenanceMode serverConfigCtx)
                       currentTime
    let cacheBuildParams =
          CacheBuildParams httpManager sourceResolver serverConfigCtx
        buildReason = case getMigratedFrom migrationResult of
          Nothing      -> CatalogSync
          Just version ->
            -- Catalog version 43 marks the metadata separation which also drops
            -- the "hdb_views" schema where table event triggers are hosted.
            -- We need to re-create table event trigger procedures in "hdb_catalog"
            -- schema when migration happens from version < 43. Build reason
            -- @'CatalogUpdate' re-creates event triggers in the database.
            if version < 43 then CatalogUpdate else CatalogSync
    schemaCache <- runCacheBuild cacheBuildParams $
                   buildRebuildableSchemaCacheWithReason buildReason env metadata
    pure (migrationResult, schemaCache)

  (migrationResult, schemaCache) <-
    initialiseResult `onLeft` \err -> do
      unLogger logger StartupLog
        { slLogLevel = LevelError
        , slKind = "catalog_migrate"
        , slInfo = A.toJSON err
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
newtype ShutdownLatch = ShutdownLatch { unShutdownLatch :: C.MVar () }

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
runHGEServer
  :: forall m impl
   . ( HasVersion
     , MonadIO m
     , MonadMask m
     , MonadStateless IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication (Tracing.TraceT m)
     , HttpLog m
     , ConsoleRenderer m
     , MonadMetadataApiAuthorization m
     , MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , MonadQueryLog m
     , WS.MonadWSLog m
     , MonadExecuteQuery m
     , Tracing.HasReporter m
     , HasResourceLimits m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadResolveSource m
     )
  => (ServerCtx -> Spock.SpockT m ())
  -> Env.Environment
  -> ServeOptions impl
  -> ServeCtx
  -- and mutations
  -> UTCTime
  -- ^ start time
  -> Maybe EL.LiveQueryPostPollHook
  -> ServerMetrics
  -> EKG.Store
  -> ManagedT m ()
runHGEServer setupHook env ServeOptions{..} ServeCtx{..} initTime postPollHook serverMetrics ekgStore = do
  -- Comment this to enable expensive assertions from "GHC.AssertNF". These
  -- will log lines to STDOUT containing "not in normal form". In the future we
  -- could try to integrate this into our tests. For now this is a development
  -- tool.
  --
  -- NOTE: be sure to compile WITHOUT code coverage, for this to work properly.
#ifndef PROFILING
  liftIO disableAssertNF
#endif

  let sqlGenCtx = SQLGenCtx soStringifyNum soDangerousBooleanCollapse
      Loggers loggerCtx logger _ = _scLoggers
      --SchemaSyncCtx{..} = _scSchemaSyncCtx

  authModeRes <- runExceptT $ setupAuthMode soAdminSecret soAuthHook soJwtSecret soUnAuthRole
                              _scHttpManager logger

  authMode <- onLeft authModeRes (printErrExit AuthConfigurationError . T.unpack)

  HasuraApp app cacheRef actionSubState stopWsServer <- lift $ flip onException (flushLogger loggerCtx) $
    mkWaiApp setupHook env
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
             soPlanCacheOptions
             soResponseInternalErrorsConfig
             postPollHook
             _scSchemaCacheRef
             ekgStore
             soEnableRemoteSchemaPermissions
             soInferFunctionPermissions
             soConnectionOptions
             soWebsocketKeepAlive
             soEnableMaintenanceMode
             soExperimentalFeatures

  let serverConfigCtx =
        ServerConfigCtx soInferFunctionPermissions
                        soEnableRemoteSchemaPermissions
                        sqlGenCtx
                        soEnableMaintenanceMode
                        soExperimentalFeatures

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSCFromRef cacheRef)
  liftIO $ logInconsObjs logger inconsObjs

  -- Start a background thread for processing schema sync event present in the '_sscSyncEventRef'
  _ <- startSchemaSyncProcessorThread logger _scHttpManager _scMetaVersionRef
                               cacheRef _scInstanceId
                               serverConfigCtx

  let
    maxEvThrds    = fromMaybe defaultMaxEventThreads soEventsHttpPoolSize
    fetchI        = milliseconds $ fromMaybe (Milliseconds defaultFetchInterval) soEventsFetchInterval
    logEnvHeaders = soLogHeadersFromEnv
    allPgSources  = mapMaybe (unsafeSourceConfiguration @'Postgres) $ HM.elems $ scSources $ lastBuiltSchemaCache _scSchemaCache

  -- TODO: is this correct?
  -- event triggers should be tied to the life cycle of a source
  lockedEventsCtx <- allocate
    (liftIO $ atomically initLockedEventsCtx)
    (\lockedEventsCtx ->
        liftWithStateless \lowerIO ->
          shutdownEvents allPgSources
          (\a b -> hoist lowerIO (unlockScheduledEvents a b)) logger lockedEventsCtx)

  -- prepare event triggers data
  eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEvThrds fetchI
  unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"

  _eventQueueThread <- C.forkManagedT "processEventQueue" logger $
    processEventQueue logger logEnvHeaders
    _scHttpManager (getSCFromRef cacheRef) eventEngineCtx lockedEventsCtx serverMetrics

  -- start a backgroud thread to handle async actions
  case soAsyncActionsFetchInterval of
    Skip -> pure () -- Don't start the poller thread
    Interval sleepTime -> do
      _asyncActionsThread <- C.forkManagedT "asyncActionsProcessor" logger $
        asyncActionsProcessor env logger (_scrCache cacheRef) _scHttpManager sleepTime
      pure ()

  -- start a backgroud thread to handle async action live queries
  _asyncActionsSubThread <- C.forkManagedT "asyncActionSubscriptionsProcessor" logger $
    asyncActionSubscriptionsProcessor actionSubState

  -- start a background thread to create new cron events
  _cronEventsThread <- C.forkManagedT "runCronEventsGenerator" logger $
    runCronEventsGenerator logger (getSCFromRef cacheRef)

  -- prepare scheduled triggers
  lift $ prepareScheduledEvents logger

  -- start a background thread to deliver the scheduled events
  _scheduledEventsThread <- C.forkManagedT "processScheduledTriggers" logger $
    processScheduledTriggers env logger logEnvHeaders _scHttpManager
                             (getSCFromRef cacheRef) lockedEventsCtx

  -- start a background thread to check for updates
  _updateThread <- C.forkManagedT "checkForUpdates" logger $ liftIO $
    checkForUpdates loggerCtx _scHttpManager

  -- start a background thread for telemetry
  dbUidE <- runMetadataStorageT getDatabaseUid
  _telemetryThread <- if soEnableTelemetry
    then do
      lift . unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice

      (dbId, pgVersion) <- liftIO $ runTxIO _scMetadataDbPool (Q.ReadCommitted, Nothing) $
        (,) <$> liftEither dbUidE <*> getPgVersion

      telemetryThread <- C.forkManagedT "runTelemetry" logger $ liftIO $
        runTelemetry logger _scHttpManager (getSCFromRef cacheRef) dbId _scInstanceId pgVersion
      return $ Just telemetryThread
    else return Nothing

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  unLogger logger $
    mkGenericLog LevelInfo "server" $ StartupTimeInfo "starting API server" apiInitTime

  let setForkIOWithMetrics :: Warp.Settings -> Warp.Settings
      setForkIOWithMetrics = Warp.setFork \f -> do
        void $ C.forkIOWithUnmask (\unmask ->
          bracket_
            (EKG.Gauge.inc $ smWarpThreads serverMetrics)
            (EKG.Gauge.dec $ smWarpThreads serverMetrics)
              (f unmask))

  let shutdownHandler closeSocket = LA.link =<< LA.async do
        waitForShutdown _scShutdownLatch
        unLogger logger $ mkGenericStrLog LevelInfo "server" "gracefully shutting down server"
        closeSocket

  let warpSettings = Warp.setPort soPort
                     . Warp.setHost soHost
                     . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
                     . Warp.setInstallShutdownHandler shutdownHandler
                     . setForkIOWithMetrics
                     $ Warp.defaultSettings

  -- Here we block until the shutdown latch 'MVar' is filled, and then
  -- shut down the server. Once this blocking call returns, we'll tidy up
  -- any resources using the finalizers attached using 'ManagedT' above.
  -- Structuring things using the shutdown latch in this way lets us decide
  -- elsewhere exactly how we want to control shutdown.
  liftIO $ Warp.runSettings warpSettings app `LE.finally` do
    -- These cleanup actions are not directly associated with any
    -- resource, but we still need to make sure we clean them up here.
    stopWsServer

  where
    -- | prepareScheduledEvents is a function to unlock all the scheduled trigger
    -- events that are locked and unprocessed, which is called while hasura is
    -- started.
    --
    -- Locked and unprocessed events can occur in 2 ways
    -- 1.
    -- Hasura's shutdown was not graceful in which all the fetched
    -- events will remain locked and unprocessed(TODO: clean shutdown)
    -- state.
    -- 2.
    -- There is another hasura instance which is processing events and
    -- it will lock events to process them.
    -- So, unlocking all the locked events might re-deliver an event(due to #2).
    prepareScheduledEvents (Logger logger) = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "scheduled_triggers" "preparing data"
      res <- runMetadataStorageT unlockAllLockedScheduledEvents
      onLeft res $ printErrJExit EventSubSystemError

    -- | shutdownEvents will be triggered when a graceful shutdown has been inititiated, it will
    -- get the locked events from the event engine context and the scheduled event engine context
    -- then it will unlock all those events.
    -- It may happen that an event may be processed more than one time, an event that has been already
    -- processed but not been marked as delivered in the db will be unlocked by `shutdownEvents`
    -- and will be processed when the events are proccessed next time.
    shutdownEvents
      :: [SourceConfig 'Postgres]
      -> (ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT IO Int)
      -> Logger Hasura
      -> LockedEventsCtx
      -> IO ()
    shutdownEvents pgSources unlockScheduledEvents' hasuraLogger@(Logger logger) LockedEventsCtx {..} = do
      forM_ pgSources $ \pgSource -> do
        logger $ mkGenericStrLog LevelInfo "event_triggers" "unlocking events that are locked by the HGE"
        let unlockEvents' l = MetadataStorageT $ runLazyTx (_pscExecCtx pgSource) Q.ReadWrite $ liftTx $ unlockEvents l
        unlockEventsForShutdown hasuraLogger "event_triggers" "" unlockEvents' leEvents
        logger $ mkGenericStrLog LevelInfo "scheduled_triggers" "unlocking scheduled events that are locked by the HGE"

      unlockEventsForShutdown hasuraLogger "scheduled_triggers" "cron events" (unlockScheduledEvents' Cron) leCronEvents
      unlockEventsForShutdown hasuraLogger "scheduled_triggers" "scheduled events" (unlockScheduledEvents' OneOff) leOneOffEvents

    unlockEventsForShutdown
      :: Logger Hasura
      -> Text -- ^ trigger type
      -> Text -- ^ event type
      -> ([eventId] -> MetadataStorageT IO Int)
      -> TVar (Set.Set eventId)
      -> IO ()
    unlockEventsForShutdown (Logger logger) triggerType eventType doUnlock lockedIdsVar = do
      lockedIds <- readTVarIO lockedIdsVar
      unless (Set.null lockedIds) $ do
        result <- runMetadataStorageT $ doUnlock $ toList lockedIds
        case result of
          Left err -> logger $ mkGenericStrLog LevelWarn triggerType $
            "Error while unlocking " ++ T.unpack eventType ++ " events: " ++ show err
          Right count -> logger $ mkGenericStrLog LevelInfo triggerType $
            show count ++ " " ++ T.unpack eventType ++ " events successfully unlocked"

runAsAdmin
  :: HTTP.Manager
  -> ServerConfigCtx
  -> RunT m a
  -> m (Either QErr a)
runAsAdmin httpManager serverConfigCtx m = do
  let runCtx = RunCtx adminUserInfo httpManager serverConfigCtx
  runExceptT $ peelRun runCtx m

execQuery
  :: ( HasVersion
     , CacheRWM m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadUnique m
     , HasHttpManagerM m
     , UserInfoM m
     , Tracing.MonadTrace m
     , HasServerConfigCtx m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => Env.Environment
  -> BLC.ByteString
  -> m BLC.ByteString
execQuery env queryBs = do
  query <- case A.decode queryBs of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"
  buildSchemaCacheStrict
  encJToLBS <$> runQueryM env query

instance Tracing.HasReporter PGMetadataStorageApp

instance HasResourceLimits PGMetadataStorageApp where
  askResourceLimits = pure (ResourceLimits id)

instance HttpLog PGMetadataStorageApp where

  type HTTPLoggingMetadata PGMetadataStorageApp = ()

  buildHTTPLoggingMetadata _ = ()

  logHttpError logger userInfoM reqId waiReq req qErr headers =
    unLogger logger $ mkHttpLog $
      mkHttpErrorLogContext userInfoM reqId waiReq req qErr Nothing Nothing headers

  logHttpSuccess logger userInfoM reqId waiReq _reqBody _response compressedResponse qTime cType headers () =
    unLogger logger $ mkHttpLog $
      mkHttpAccessLogContext userInfoM reqId waiReq compressedResponse qTime cType headers

instance MonadExecuteQuery PGMetadataStorageApp where
  cacheLookup _ _ _ = pure ([], Nothing)
  cacheStore  _ _ = pure ()

instance UserAuthentication (Tracing.TraceT PGMetadataStorageApp) where
  resolveUserInfo logger manager headers authMode reqs =
    runExceptT $ getUserInfoWithExpTime logger manager headers authMode reqs

accessDeniedErrMsg :: Text
accessDeniedErrMsg =
  "restricted access : admin only"

instance MonadMetadataApiAuthorization PGMetadataStorageApp where
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

instance ConsoleRenderer PGMetadataStorageApp where
  renderConsole path authMode enableTelemetry consoleAssetsDir =
    return $ mkConsoleHTML path authMode enableTelemetry consoleAssetsDir

instance MonadGQLExecutionCheck PGMetadataStorageApp where
  checkGQLExecution userInfo _ enableAL sc query = runExceptT $ do
    req <- toParsed query
    checkQueryInAllowlist enableAL userInfo req sc
    return req

instance MonadConfigApiHandler PGMetadataStorageApp where
  runConfigApiHandler = configApiGetHandler

instance MonadQueryLog PGMetadataStorageApp where
  logQueryLog = unLogger

instance WS.MonadWSLog PGMetadataStorageApp where
  logWSLog = unLogger

instance MonadResolveSource PGMetadataStorageApp where
  getSourceResolver = mkPgSourceResolver <$> asks snd

runInSeparateTx :: Q.TxE QErr a -> MetadataStorageT PGMetadataStorageApp a
runInSeparateTx tx = do
  pool <- lift $ asks fst
  liftEitherM $ liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Nothing) tx

notifySchemaCacheSyncTx :: MetadataResourceVersion -> InstanceId -> CacheInvalidations -> Q.TxE QErr ()
notifySchemaCacheSyncTx (MetadataResourceVersion resourceVersion) instanceId invalidations = do
  Q.Discard () <- Q.withQE defaultTxErrorHandler [Q.sql|
      INSERT INTO hdb_catalog.hdb_schema_notifications(id, notification, resource_version, instance_id)
      VALUES (1, $1::json, $2, $3::uuid)
      ON CONFLICT (id) DO UPDATE SET
        notification = $1::json,
        resource_version = $2,
        instance_id = $3::uuid
    |] (Q.AltJ invalidations, resourceVersion, instanceId) True
  pure ()

getCatalogStateTx :: Q.TxE QErr CatalogState
getCatalogStateTx =
  mkCatalogState . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
    SELECT hasura_uuid::text, cli_state::json, console_state::json
      FROM hdb_catalog.hdb_version
  |] () False
  where
    mkCatalogState (dbId, Q.AltJ cliState, Q.AltJ consoleState) =
      CatalogState dbId cliState consoleState

setCatalogStateTx :: CatalogStateType -> A.Value -> Q.TxE QErr ()
setCatalogStateTx stateTy stateValue =
  case stateTy of
    CSTCli ->
      Q.unitQE defaultTxErrorHandler [Q.sql|
        UPDATE hdb_catalog.hdb_version
           SET cli_state = $1
      |] (Identity $ Q.AltJ stateValue) False
    CSTConsole ->
      Q.unitQE defaultTxErrorHandler [Q.sql|
        UPDATE hdb_catalog.hdb_version
           SET console_state = $1
      |] (Identity $ Q.AltJ stateValue) False


-- | Each of the function in the type class is executed in a totally separate transaction.
--
-- To learn more about why the instance is derived as following, see Note [Generic MetadataStorageT transformer]
instance MonadMetadataStorage (MetadataStorageT PGMetadataStorageApp) where

  fetchMetadataResourceVersion   = runInSeparateTx fetchMetadataResourceVersionFromCatalog
  fetchMetadata                  = runInSeparateTx fetchMetadataAndResourceVersionFromCatalog
  fetchMetadataNotifications a b = runInSeparateTx $ fetchMetadataNotificationsFromCatalog a b
  setMetadata r                  = runInSeparateTx . setMetadataInCatalog (Just r)
  notifySchemaCacheSync a b c    = runInSeparateTx $ notifySchemaCacheSyncTx a b c
  getCatalogState     = runInSeparateTx getCatalogStateTx
  setCatalogState a b = runInSeparateTx $ setCatalogStateTx a b

  getDatabaseUid      = runInSeparateTx getDbId
  checkMetadataStorageHealth = lift (asks fst) >>= checkDbConnection

  getDeprivedCronTriggerStats        = runInSeparateTx getDeprivedCronTriggerStatsTx
  getScheduledEventsForDelivery      = runInSeparateTx getScheduledEventsForDeliveryTx
  insertScheduledEvent               = runInSeparateTx . insertScheduledEventTx
  insertScheduledEventInvocation a b = runInSeparateTx $ insertInvocationTx a b
  setScheduledEventOp a b c          = runInSeparateTx $ setScheduledEventOpTx a b c
  unlockScheduledEvents a b          = runInSeparateTx $ unlockScheduledEventsTx a b
  unlockAllLockedScheduledEvents     = runInSeparateTx unlockAllLockedScheduledEventsTx
  clearFutureCronEvents              = runInSeparateTx . dropFutureCronEventsTx
  getOneOffScheduledEvents a b       = runInSeparateTx $ getOneOffScheduledEventsTx a b
  getCronEvents a b c                = runInSeparateTx $ getCronEventsTx a b c
  getInvocations a b                 = runInSeparateTx $ getInvocationsTx a b
  deleteScheduledEvent a b           = runInSeparateTx $ deleteScheduledEventTx a b

  insertAction a b c d         = runInSeparateTx $ insertActionTx a b c d
  fetchUndeliveredActionEvents = runInSeparateTx fetchUndeliveredActionEventsTx
  setActionStatus a b          = runInSeparateTx $ setActionStatusTx a b
  fetchActionResponse          = runInSeparateTx . fetchActionResponseTx
  clearActionData              = runInSeparateTx . clearActionDataTx

--- helper functions ---

mkConsoleHTML :: HasVersion => Text -> AuthMode -> Bool -> Maybe Text -> Either String Text
mkConsoleHTML path authMode enableTelemetry consoleAssetsDir =
  renderHtmlTemplate consoleTmplt $
      -- variables required to render the template
      A.object [ "isAdminSecretSet" A..= isAdminSecretSet authMode
               , "consolePath"      A..= consolePath
               , "enableTelemetry"  A..= boolToText enableTelemetry
               , "cdnAssets"        A..= boolToText (isNothing consoleAssetsDir)
               , "assetsVersion"    A..= consoleAssetsVersion
               , "serverVersion"    A..= currentVersion
               ]
    where
      consolePath = case path of
        "" -> "/console"
        r  -> "/console/" <> r

      consoleTmplt = $(makeRelativeToProject "src-rsr/console.html" >>= M.embedSingleTemplate)

telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
  <> "usage stats which allows us to keep improving Hasura at warp speed. "
  <> "To read more or opt-out, visit https://hasura.io/docs/latest/graphql/core/guides/telemetry.html"
