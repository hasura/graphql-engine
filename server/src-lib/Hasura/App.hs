{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.App where

import           Control.Concurrent.STM.TVar               (TVar, readTVarIO)
import           Control.Exception                         (throwIO)
import           Control.Monad.Base
import           Control.Monad.Catch                       (Exception, MonadCatch, MonadMask,
                                                            MonadThrow, onException)
import           Control.Monad.Morph                       (hoist)
import           Control.Monad.Stateless
import           Control.Monad.STM                         (atomically)
import           Control.Monad.Trans.Control               (MonadBaseControl (..))
import           Control.Monad.Unique
import           Data.Aeson                                ((.=))
import           Data.Time.Clock                           (UTCTime)
#ifndef PROFILING
import           GHC.AssertNF
#endif
import           GHC.Stats
import           Options.Applicative
import           System.Environment                        (getEnvironment)
import           System.Mem                                (performMajorGC)

import qualified Control.Concurrent.Async.Lifted.Safe      as LA
import qualified Control.Concurrent.Extended               as C
import qualified Control.Immortal                          as Immortal
import qualified Data.Aeson                                as A
import qualified Data.ByteString.Char8                     as BC
import qualified Data.ByteString.Lazy.Char8                as BLC
import qualified Data.Environment                          as Env
import qualified Data.Set                                  as Set
import qualified Data.Text                                 as T
import qualified Data.Time.Clock                           as Clock
import qualified Data.Yaml                                 as Y
import qualified Database.PG.Query                         as Q
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Client.TLS                   as HTTP
import qualified Network.Wai.Handler.Warp                  as Warp
import qualified System.Log.FastLogger                     as FL
import qualified Text.Mustache.Compile                     as M

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.Eventing.Common
import           Hasura.Eventing.EventTrigger
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.GraphQL.Execute                    (MonadGQLExecutionCheck (..),
                                                            checkQueryInAllowlist)
import           Hasura.GraphQL.Execute.Action             (asyncActionsProcessor)
import           Hasura.GraphQL.Execute.Query              (MonadQueryInstrumentation (..),
                                                            noProfile)
import           Hasura.GraphQL.Logging                    (MonadQueryLog (..), QueryLog (..))
import           Hasura.GraphQL.Transport.HTTP             (MonadExecuteQuery (..))
import           Hasura.GraphQL.Transport.HTTP.Protocol    (toParsed)
import           Hasura.Logging
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.API.Query                   (requiresAdmin, runQueryM)
import           Hasura.Server.App
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates                (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Migrate                     (migrateCatalog)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Types
import           Hasura.Server.Version
import           Hasura.Session

import qualified Hasura.GraphQL.Execute.LiveQuery.Poll     as EL
import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS
import qualified Hasura.Tracing                            as Tracing
import qualified System.Metrics                            as EKG


data ExitCode
  = InvalidEnvironmentVariableOptionsError
  | InvalidDatabaseConnectionParamsError
  | MetadataCatalogFetchingError
  | AuthConfigurationError
  | EventSubSystemError
  | EventEnvironmentVariableError
  | MetadataExportError
  | MetadataCleanError
  | DatabaseMigrationError
  | ExecuteProcessError
  | DowngradeProcessError
  | UnexpectedHasuraError
  | ExitFailureError Int
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
    hgeOpts = HGEOptionsG <$> parseRawConnInfo <*> parseHGECommand

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
  { _gcHttpManager :: !HTTP.Manager
  , _gcConnInfo    :: !Q.ConnInfo
  }

initGlobalCtx
  :: (MonadIO m) => RawConnInfo -> m GlobalCtx
initGlobalCtx rawConnInfo = do
  _gcHttpManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  _gcConnInfo <- liftIO $ onLeft (mkConnInfo rawConnInfo) $
    printErrExit InvalidDatabaseConnectionParamsError . ("Fatal Error : " <>)
  pure GlobalCtx{..}


-- | Context required for the 'serve' CLI command.
data ServeCtx
  = ServeCtx
  { _scHttpManager   :: !HTTP.Manager
  , _scInstanceId    :: !InstanceId
  , _scLoggers       :: !Loggers
  , _scConnInfo      :: !Q.ConnInfo
  , _scPgPool        :: !Q.PGPool
  , _scShutdownLatch :: !ShutdownLatch
  , _scSchemaCache   :: !(RebuildableSchemaCache Run)
  , _scSchemaSyncCtx :: !SchemaSyncCtx
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
  = PGMetadataStorageApp {runPGMetadataStorageApp :: Q.PGPool -> IO a}
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadBase IO, MonadBaseControl IO
           , MonadCatch, MonadThrow, MonadMask
           , MonadUnique, MonadReader Q.PGPool
           ) via (ReaderT Q.PGPool IO)

-- | Initializes or migrates the catalog and returns the context required to start the server.
initialiseServeCtx
  :: (HasVersion, MonadIO m, MonadCatch m)
  => Env.Environment
  -> GlobalCtx
  -> ServeOptions Hasura
  -> m ServeCtx
initialiseServeCtx env GlobalCtx{..} so@ServeOptions{..} = do
  instanceId <- liftIO generateInstanceId
  latch <- liftIO newShutdownLatch
  loggers@(Loggers loggerCtx logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel
  -- log serve options
  unLogger logger $ serveOptsToLog so
  -- log postgres connection info
  unLogger logger $ connInfoToLog _gcConnInfo
  pool <- liftIO $ Q.initPGPool _gcConnInfo soConnParams pgLogger
  let sqlGenCtx = SQLGenCtx soStringifyNum

  -- Start a background thread for listening schema sync events from other server instances,
  -- just before building @'RebuildableSchemaCache' (happens in @'migrateCatalogSchema' function).
  -- See Note [Schema Cache Sync]
  (schemaSyncListenerThread, schemaSyncEventRef) <- startSchemaSyncListenerThread pool logger instanceId

  (rebuildableSchemaCache, cacheInitStartTime) <-
    flip onException (flushLogger loggerCtx) $ migrateCatalogSchema env logger pool _gcHttpManager sqlGenCtx

  let schemaSyncCtx = SchemaSyncCtx schemaSyncListenerThread schemaSyncEventRef cacheInitStartTime
      initCtx = ServeCtx _gcHttpManager instanceId loggers _gcConnInfo pool latch
                rebuildableSchemaCache schemaSyncCtx
  pure initCtx

mkLoggers
  :: (MonadIO m)
  => HashSet (EngineLogType Hasura) -> LogLevel -> m Loggers
mkLoggers enabledLogs logLevel = do
  loggerCtx <- liftIO $ mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
  let logger = mkLogger loggerCtx
      pgLogger = mkPGLogger logger
  return $ Loggers loggerCtx logger pgLogger


-- | helper function to initialize or migrate the @hdb_catalog@ schema (used by pro as well)
migrateCatalogSchema
  :: (HasVersion, MonadIO m)
  => Env.Environment -> Logger Hasura -> Q.PGPool -> HTTP.Manager -> SQLGenCtx
  -> m (RebuildableSchemaCache Run, UTCTime)
migrateCatalogSchema env logger pool httpManager sqlGenCtx = do
  let pgExecCtx = mkPGExecCtx Q.Serializable pool
      adminRunCtx = RunCtx adminUserInfo httpManager sqlGenCtx
  currentTime <- liftIO Clock.getCurrentTime
  initialiseResult <- runExceptT $
                      peelRun adminRunCtx pgExecCtx Q.ReadWrite Nothing $
                      migrateCatalog env currentTime

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
shutdownGracefully = flip C.putMVar () . unShutdownLatch

-- | If an exception is encountered , flush the log buffer and
-- rethrow If we do not flush the log buffer on exception, then log lines
-- may be missed
-- See: https://github.com/hasura/graphql-engine/issues/4772
flushLogger :: MonadIO m => LoggerCtx impl -> m ()
flushLogger = liftIO . FL.flushLogStr . _lcLoggerSet

runHGEServer
  :: ( HasVersion
     , MonadIO m
     , MonadMask m
     , MonadStateless IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication (Tracing.TraceT m)
     , HttpLog m
     , ConsoleRenderer m
     , MetadataApiAuthorization m
     , MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , MonadQueryLog m
     , WS.MonadWSLog m
     , MonadExecuteQuery m
     , Tracing.HasReporter m
     , MonadQueryInstrumentation m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> ServeOptions impl
  -> ServeCtx
  -> Maybe PGExecCtx
  -- ^ An optional specialized pg exection context for executing queries
  -- and mutations
  -> UTCTime
  -- ^ start time
  -> IO ()
  -- ^ shutdown function
  -> Maybe EL.LiveQueryPostPollHook
  -> EKG.Store
  -> m ()
runHGEServer env ServeOptions{..} ServeCtx{..} pgExecCtx initTime shutdownApp postPollHook ekgStore = do
  -- Comment this to enable expensive assertions from "GHC.AssertNF". These
  -- will log lines to STDOUT containing "not in normal form". In the future we
  -- could try to integrate this into our tests. For now this is a development
  -- tool.
  --
  -- NOTE: be sure to compile WITHOUT code coverage, for this to work properly.
#ifndef PROFILING
  liftIO disableAssertNF
#endif

  let sqlGenCtx = SQLGenCtx soStringifyNum
      Loggers loggerCtx logger _ = _scLoggers
      SchemaSyncCtx{..} = _scSchemaSyncCtx

  authModeRes <- runExceptT $ setupAuthMode soAdminSecret soAuthHook soJwtSecret soUnAuthRole
                              _scHttpManager logger

  authMode <- onLeft authModeRes (printErrExit AuthConfigurationError . T.unpack)

  _idleGCThread <- C.forkImmortal "ourIdleGC" logger $ liftIO $
    ourIdleGC logger (seconds 0.3) (seconds 10) (seconds 60)

  HasuraApp app cacheRef stopWsServer <- flip onException (flushLogger loggerCtx) $
    mkWaiApp env
             soTxIso
             logger
             sqlGenCtx
             soEnableAllowlist
             _scPgPool
             pgExecCtx
             _scConnInfo
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
             _scSchemaCache
             ekgStore
             soConnectionOptions
             soWebsocketKeepAlive

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSCFromRef cacheRef)
  liftIO $ logInconsObjs logger inconsObjs

  -- Start a background thread for processing schema sync event present in the '_sscSyncEventRef'
  schemaSyncProcessorThread <- startSchemaSyncProcessorThread sqlGenCtx _scPgPool
                               logger _scHttpManager _sscSyncEventRef
                               cacheRef _scInstanceId _sscCacheInitStartTime

  let
    maxEvThrds    = fromMaybe defaultMaxEventThreads soEventsHttpPoolSize
    fetchI        = milliseconds $ fromMaybe (Milliseconds defaultFetchInterval) soEventsFetchInterval
    logEnvHeaders = soLogHeadersFromEnv

  lockedEventsCtx <- liftIO $ atomically initLockedEventsCtx

  -- prepare event triggers data
  eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEvThrds fetchI
  unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"

  eventQueueThread <- C.forkImmortal "processEventQueue" logger $
    processEventQueue logger logEnvHeaders
    _scHttpManager _scPgPool (getSCFromRef cacheRef) eventEngineCtx lockedEventsCtx

  -- start a backgroud thread to handle async actions
  asyncActionsThread <- C.forkImmortal "asyncActionsProcessor" logger $
    asyncActionsProcessor env logger (_scrCache cacheRef) _scPgPool _scHttpManager

  -- start a background thread to create new cron events
  cronEventsThread <- C.forkImmortal "runCronEventsGenerator" logger $
    runCronEventsGenerator logger (getSCFromRef cacheRef)

  -- prepare scheduled triggers
  prepareScheduledEvents logger

  -- start a background thread to deliver the scheduled events
  scheduledEventsThread <- C.forkImmortal "processScheduledTriggers" logger $
    processScheduledTriggers env logger logEnvHeaders _scHttpManager (getSCFromRef cacheRef) lockedEventsCtx

  -- start a background thread to check for updates
  updateThread <- C.forkImmortal "checkForUpdates" logger $ liftIO $
    checkForUpdates loggerCtx _scHttpManager

  -- start a background thread for telemetry
  telemetryThread <- if soEnableTelemetry
    then do
      unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice

      (dbId, pgVersion) <- liftIO $ runTxIO _scPgPool (Q.ReadCommitted, Nothing) $
        (,) <$> getDbId <*> getPgVersion

      telemetryThread <- C.forkImmortal "runTelemetry" logger $ liftIO $
        runTelemetry logger _scHttpManager (getSCFromRef cacheRef) dbId _scInstanceId pgVersion
      return $ Just telemetryThread
    else return Nothing

  -- all the immortal threads are collected so that they can be stopped when gracefully shutting down
  let immortalThreads = [ _sscListenerThreadId
                        , schemaSyncProcessorThread
                        , updateThread
                        , asyncActionsThread
                        , eventQueueThread
                        , scheduledEventsThread
                        , cronEventsThread
                        ] <> onNothing telemetryThread []

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  unLogger logger $
    mkGenericLog LevelInfo "server" $ StartupTimeInfo "starting API server" apiInitTime

  shutdownHandler' <- liftWithStateless $ \lowerIO ->
    pure $ shutdownHandler _scLoggers immortalThreads stopWsServer lockedEventsCtx _scPgPool $
           \a b -> hoist lowerIO $ unlockScheduledEvents a b
  let warpSettings = Warp.setPort soPort
                     . Warp.setHost soHost
                     . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
                     . Warp.setInstallShutdownHandler shutdownHandler'
                     $ Warp.defaultSettings
  liftIO $ Warp.runSettings warpSettings app

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
      :: Q.PGPool
      -> (ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT IO Int)
      -> Logger Hasura
      -> LockedEventsCtx
      -> IO ()
    shutdownEvents pool unlockScheduledEvents' hasuraLogger@(Logger logger) LockedEventsCtx {..} = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "event_triggers" "unlocking events that are locked by the HGE"
      let unlockEvents' =
            liftEitherM . liftIO . runTx pool (Q.ReadCommitted, Nothing) . unlockEvents
      unlockEventsForShutdown hasuraLogger "event_triggers" "" unlockEvents' leEvents
      liftIO $ logger $ mkGenericStrLog LevelInfo "scheduled_triggers" "unlocking scheduled events that are locked by the HGE"
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

    runTx :: Q.PGPool -> Q.TxMode -> Q.TxE QErr a -> IO (Either QErr a)
    runTx pool txLevel tx =
      liftIO $ runExceptT $ Q.runTx pool txLevel tx

    -- | Waits for the shutdown latch 'MVar' to be filled, and then
    -- shuts down the server and associated resources.
    -- Structuring things this way lets us decide elsewhere exactly how
    -- we want to control shutdown.
    shutdownHandler
      :: Loggers
      -> [Immortal.Thread]
      -> IO ()
      -- ^ the stop websocket server function
      -> LockedEventsCtx
      -> Q.PGPool
      -> (ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT IO Int)
      -> IO ()
      -- ^ the closeSocket callback
      -> IO ()
    shutdownHandler (Loggers loggerCtx (Logger logger) _) immortalThreads stopWsServer leCtx pool unlockScheduledEvents' closeSocket =
      LA.link =<< LA.async do
        waitForShutdown _scShutdownLatch
        logger $ mkGenericStrLog LevelInfo "server" "gracefully shutting down server"
        shutdownEvents pool unlockScheduledEvents' (Logger logger) leCtx
        closeSocket
        stopWsServer
        -- kill all the background immortal threads
        logger $ mkGenericStrLog LevelInfo "server" "killing all background immortal threads"
        forM_ immortalThreads $ \thread -> do
          logger $ mkGenericStrLog LevelInfo "server" $ "killing thread: " <> show (Immortal.threadId thread)
          Immortal.stop thread
        shutdownApp
        cleanLoggerCtx loggerCtx

-- | The RTS's idle GC doesn't work for us:
--
--    - when `-I` is too low it may fire continuously causing scary high CPU
--      when idle among other issues (see #2565)
--    - when we set it higher it won't run at all leading to memory being
--      retained when idle (especially noticeable when users are benchmarking and
--      see memory stay high after finishing). In the theoretical worst case
--      there is such low haskell heap pressure that we never run finalizers to
--      free the foreign data from e.g. libpq.
--    - as of GHC 8.10.2 we have access to `-Iw`, but those two knobs still
--      don’t give us a guarantee that a major GC will always run at some
--      minumum frequency (e.g. for finalizers)
--
-- ...so we hack together our own using GHC.Stats, which should have
-- insignificant runtime overhead.
ourIdleGC
  :: Logger Hasura
  -> DiffTime -- ^ Run a major GC when we've been "idle" for idleInterval
  -> DiffTime -- ^ ...as long as it has been > minGCInterval time since the last major GC
  -> DiffTime -- ^ Additionally, if it has been > maxNoGCInterval time, force a GC regardless.
  -> IO void
ourIdleGC (Logger logger) idleInterval minGCInterval maxNoGCInterval =
  startTimer >>= go 0 0
  where
    go gcs_prev major_gcs_prev timerSinceLastMajorGC = do
      timeSinceLastGC <- timerSinceLastMajorGC
      when (timeSinceLastGC < minGCInterval) $ do
        -- no need to check idle until we've passed the minGCInterval:
        C.sleep (minGCInterval - timeSinceLastGC)

      RTSStats{gcs, major_gcs} <- getRTSStats
      -- We use minor GCs as a proxy for "activity", which seems to work
      -- well-enough (in tests it stays stable for a few seconds when we're
      -- logically "idle" and otherwise increments quickly)
      let areIdle = gcs == gcs_prev
          areOverdue = timeSinceLastGC > maxNoGCInterval

         -- a major GC was run since last iteration (cool!), reset timer:
      if | major_gcs > major_gcs_prev -> do
             startTimer >>= go gcs major_gcs

         -- we are idle and its a good time to do a GC, or we're overdue and must run a GC:
         | areIdle || areOverdue -> do
             when (areOverdue && not areIdle) $
               logger $ UnstructuredLog LevelWarn $
                 "Overdue for a major GC: forcing one even though we don't appear to be idle"
             performMajorGC
             startTimer >>= go (gcs+1) (major_gcs+1)

         -- else keep the timer running, waiting for us to go idle:
         | otherwise -> do
             C.sleep idleInterval
             go gcs major_gcs timerSinceLastMajorGC

runAsAdmin
  :: (MonadIO m)
  => Q.PGPool
  -> SQLGenCtx
  -> HTTP.Manager
  -> Run a
  -> m (Either QErr a)
runAsAdmin pool sqlGenCtx httpManager m = do
  let runCtx = RunCtx adminUserInfo httpManager sqlGenCtx
      pgCtx = mkPGExecCtx Q.Serializable pool
  runExceptT $ peelRun runCtx pgCtx Q.ReadWrite Nothing m

execQuery
  :: ( HasVersion
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , MonadUnique m
     , HasHttpManager m
     , HasSQLGenCtx m
     , UserInfoM m
     , HasSystemDefined m
     , Tracing.MonadTrace m
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

instance MonadQueryInstrumentation PGMetadataStorageApp where
  askInstrumentQuery _ = pure (id, noProfile)

instance HttpLog PGMetadataStorageApp where
  logHttpError logger userInfoM reqId waiReq req qErr headers =
    unLogger logger $ mkHttpLog $
      mkHttpErrorLogContext userInfoM reqId waiReq req qErr Nothing Nothing headers

  logHttpSuccess logger userInfoM reqId waiReq _reqBody _response compressedResponse qTime cType headers =
    unLogger logger $ mkHttpLog $
      mkHttpAccessLogContext userInfoM reqId waiReq compressedResponse qTime cType headers

instance MonadExecuteQuery PGMetadataStorageApp where
  cacheLookup _ _ _ = pure ([], Nothing)
  cacheStore  _ _ = pure ()

instance UserAuthentication (Tracing.TraceT PGMetadataStorageApp) where
  resolveUserInfo logger manager headers authMode =
    runExceptT $ getUserInfoWithExpTime logger manager headers authMode

instance MetadataApiAuthorization PGMetadataStorageApp where
  authorizeMetadataApi query userInfo = do
    let currRole = _uiRole userInfo
    when (requiresAdmin query && currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied errMsg
    where
      errMsg = "restricted access : admin only"

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
  logQueryLog logger query genSqlM reqId =
    unLogger logger $ QueryLog query genSqlM reqId

instance WS.MonadWSLog PGMetadataStorageApp where
  logWSLog = unLogger

runInSeparateTx :: Q.TxE QErr a -> MetadataStorageT PGMetadataStorageApp a
runInSeparateTx tx = do
  pool <- lift ask
  liftEitherM $ liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Nothing) tx

-- | Each of the function in the type class is executed in a totally separate transaction.
--
-- To learn more about why the instance is derived as following, see Note [Generic MetadataStorageT transformer]
instance MonadMetadataStorage (MetadataStorageT PGMetadataStorageApp) where

  getDeprivedCronTriggerStats        = runInSeparateTx getDeprivedCronTriggerStatsTx
  getScheduledEventsForDelivery      = runInSeparateTx getScheduledEventsForDeliveryTx
  insertScheduledEvent               = runInSeparateTx . insertScheduledEventTx
  insertScheduledEventInvocation a b = runInSeparateTx $ insertInvocationTx a b
  setScheduledEventOp a b c          = runInSeparateTx $ setScheduledEventOpTx a b c
  unlockScheduledEvents a b          = runInSeparateTx $ unlockScheduledEventsTx a b
  unlockAllLockedScheduledEvents     = runInSeparateTx unlockAllLockedScheduledEventsTx

--- helper functions ---

mkConsoleHTML :: HasVersion => Text -> AuthMode -> Bool -> Maybe Text -> Either String Text
mkConsoleHTML path authMode enableTelemetry consoleAssetsDir =
  renderHtmlTemplate consoleTmplt $
      -- variables required to render the template
      A.object [ "isAdminSecretSet" .= isAdminSecretSet authMode
               , "consolePath" .= consolePath
               , "enableTelemetry" .= boolToText enableTelemetry
               , "cdnAssets" .= boolToText (isNothing consoleAssetsDir)
               , "assetsVersion" .= consoleAssetsVersion
               , "serverVersion" .= currentVersion
               ]
    where
      consolePath = case path of
        "" -> "/console"
        r  -> "/console/" <> r

      consoleTmplt = $(M.embedSingleTemplate "src-rsr/console.html")

telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
  <> "usage stats which allows us to keep improving Hasura at warp speed. "
  <> "To read more or opt-out, visit https://hasura.io/docs/1.0/graphql/manual/guides/telemetry.html"
