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
import qualified Data.HashMap.Strict                       as HM
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

import           Hasura.Class
import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Eventing.Common
import           Hasura.Eventing.EventTrigger
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.GraphQL.Execute                    (MonadGQLExecutionCheck (..),
                                                            checkQueryInAllowlist)
import           Hasura.GraphQL.Execute.Action             (asyncActionsProcessor,
                                                            fetchActionResponseTx, insertActionTx,
                                                            setActionStatusTx, undeliveredEventsTx)
import           Hasura.GraphQL.Execute.Query              (MonadQueryInstrumentation (..),
                                                            noProfile)
import           Hasura.GraphQL.Logging                    (MonadQueryLog (..), QueryLog (..))
import           Hasura.GraphQL.Transport.HTTP             (MonadExecuteQuery (..))
import           Hasura.GraphQL.Transport.HTTP.Protocol    (toParsed)
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.API.Query
import           Hasura.Server.App
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates                (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Migrate                     (fetchMetadataTx,
                                                            migrateMetadataStorageCatalog,
                                                            setMetadataTx)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Types
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.Session

import qualified Hasura.GraphQL.Execute.LiveQuery.Poll     as EL
import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS
import qualified Hasura.Tracing                            as Tracing
import qualified System.Metrics                            as EKG


data ExitCode
  = InvalidEnvironmentVariableOptionsError
  | InvalidDatabaseConnectionParamsError
  | CacheBuildError
  | MetadataCatalogFetchingError
  | MetadataDbUidFetchError
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
  either (printErrExit InvalidEnvironmentVariableOptionsError) return eitherOpts
  where
    opts = info (helper <*> hgeOpts)
           ( fullDesc <>
             header "Hasura GraphQL Engine: Realtime GraphQL API over Postgres with access control" <>
             footerDoc (Just mainCmdFooter)
           )
    hgeOpts = HGEOptionsG <$> parseDefaultConnInfo <*> parseMetadataDbUrl <*> parseHGECommand

printJSON :: (A.ToJSON a, MonadIO m) => a -> m ()
printJSON = liftIO . BLC.putStrLn . A.encode

printYaml :: (A.ToJSON a, MonadIO m) => a -> m ()
printYaml = liftIO . BC.putStrLn . Y.encode

mkPGLogger :: Logger Hasura -> Q.PGLogger
mkPGLogger (Logger logger) (Q.PLERetryMsg msg) =
  logger $ PGLog LevelWarn msg


-- | most of the required types for initializing graphql-engine
data InitCtx
  = InitCtx
  { _icHttpManager   :: !HTTP.Manager
  , _icInstanceId    :: !InstanceId
  , _icLoggers       :: !Loggers
  , _icShutdownLatch :: !ShutdownLatch
  , _icSchemaCache   :: !RebuildableSchemaCache
  , _icMetadata      :: !Metadata
  , _icMetadataPool  :: !Q.PGPool
  }

-- | Collection of the LoggerCtx, the regular Logger and the PGLogger
-- TODO (from master): better naming?
data Loggers
  = Loggers
  { _lsLoggerCtx :: !(LoggerCtx Hasura)
  , _lsLogger    :: !(Logger Hasura)
  , _lsPgLogger  :: !Q.PGLogger
  }

newtype ServerAppM a = ServerAppM { unServerAppM :: ReaderT Q.PGPool IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadBase IO, MonadBaseControl IO
           , MonadCatch, MonadThrow, MonadMask
           , MonadReader Q.PGPool
           , MonadUnique
           )

-- | this function initializes the catalog and returns an @InitCtx@, based on the command given
-- - for serve command it creates a proper PG connection pool
-- - for other commands, it creates a minimal pool
-- this exists as a separate function because the context (logger, http manager, pg pool) can be
-- used by other functions as well
initialiseCtx
  :: (HasVersion, MonadIO m, MonadCatch m)
  => Env.Environment
  -> HGEOptions Hasura
  -> m (InitCtx, UTCTime)
initialiseCtx env (HGEOptionsG rci metadataDbUrl hgeCmd) = do
  initTime <- liftIO Clock.getCurrentTime
  -- global http manager
  httpManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  instanceId <- liftIO generateInstanceId
  latch <- liftIO newShutdownLatch
  (loggers, sqlGenCtx, connParams) <- case hgeCmd of
    -- for the @serve@ command generate a regular PG pool
    HCServe so@ServeOptions{..} -> do
      l <- mkLoggers soEnabledLogTypes soLogLevel

      -- log serve options
      unLogger (_lsLogger l) $ serveOptsToLog so

      pure (l, SQLGenCtx soStringifyNum, soConnParams)

    -- for other commands generate a minimal PG pool
    _ -> do
      l <- mkLoggers defaultEnabledLogTypes LevelInfo
      pure (l, SQLGenCtx False, Q.defaultConnParams{Q.cpConns = 1})

  let Loggers loggerCtx logger pgLogger = loggers

  (metadata, metadataPool) <-
    flip onException (flushLogger loggerCtx) $
    migrateCatalogSchema logger pgLogger env connParams rci metadataDbUrl

  schemaCacheE <- runExceptT
    $ peelMetadataRun (RunCtx adminUserInfo httpManager sqlGenCtx) metadata
    $ buildRebuildableSchemaCache env

  schemaCache <- fmap fst $ onLeft schemaCacheE $ \err -> do
    unLogger logger StartupLog
        { slLogLevel = LevelError
        , slKind = "cache_build"
        , slInfo = A.toJSON err
        }
    liftIO $ printErrExit CacheBuildError $ BLC.unpack $ A.encode err

  pure (InitCtx httpManager instanceId loggers latch schemaCache metadata metadataPool, initTime)
  where
    mkLoggers enabledLogs logLevel = do
      loggerCtx <- liftIO $ mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
      let logger = mkLogger loggerCtx
          pgLogger = mkPGLogger logger
      return $ Loggers loggerCtx logger pgLogger

-- | helper function to initialize or migrate the @hdb_catalog@ schema (used by pro as well)
migrateCatalogSchema
  :: (MonadIO m)
  => Logger Hasura
  -> Q.PGLogger
  -> Env.Environment
  -> Q.ConnParams
  -> Maybe (DefaultConnInfo UrlConf)
  -> Maybe String
  -> m (Metadata, Q.PGPool)
migrateCatalogSchema logger pgLogger env connParams dbUrl mdUrl = do
      -- adminRunCtx = RunCtx adminUserInfo httpManager sqlGenCtx
  currentTime <- liftIO Clock.getCurrentTime
  eitherResult <- liftIO $ runExceptT $
    migrateMetadataStorageCatalog env connParams logger pgLogger dbUrl mdUrl currentTime

  (migrationResult, metadata, metadataPool) <- case eitherResult of
    Left err -> do
      unLogger logger StartupLog
        { slLogLevel = LevelError
        , slKind = "catalog_migrate"
        , slInfo = A.toJSON err
        }
      liftIO (printErrExit DatabaseMigrationError (BLC.unpack $ A.encode err))
    Right a -> pure a
  unLogger logger migrationResult
  pure (metadata, metadataPool)

-- | Run a transaction and if an error is encountered, log the error and abort the program
runTxIO :: Q.PGPool -> Q.TxMode -> Q.TxE QErr a -> IO a
runTxIO pool isoLevel tx = do
  eVal <- liftIO $ runExceptT $ Q.runTx pool isoLevel tx
  either (printErrJExit DatabaseMigrationError) return eVal

-- | A latch for the graceful shutdown of a server process.
newtype ShutdownLatch = ShutdownLatch { unShutdownLatch :: C.MVar () }

newShutdownLatch :: IO ShutdownLatch
newShutdownLatch = fmap ShutdownLatch C.newEmptyMVar

-- | Block the current thread, waiting on the latch.
waitForShutdown :: ShutdownLatch -> IO ()
waitForShutdown = C.takeMVar . unShutdownLatch

-- | Initiate a graceful shutdown of the server associated with the provided
-- latch.
shutdownGracefully :: InitCtx -> IO ()
shutdownGracefully = shutdownGracefully' . _icShutdownLatch

shutdownGracefully' :: ShutdownLatch -> IO ()
shutdownGracefully' = flip C.putMVar () . unShutdownLatch

-- | If an exception is encountered , flush the log buffer and
-- rethrow If we do not flush the log buffer on exception, then log lines
-- may be missed
-- See: https://github.com/hasura/graphql-engine/issues/4772
flushLogger :: MonadIO m => LoggerCtx impl -> m ()
flushLogger = liftIO . FL.flushLogStr . _lcLoggerSet

runHGEServer
  :: forall m impl
  .  ( HasVersion
     , MonadIO m
     , MonadMask m
     , MonadUnique m
     , MonadStateless IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication (Tracing.TraceT m)
     , HttpLog m
     , ConsoleRenderer m
     , MonadApiAuthorization m
     , MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , MonadQueryLog m
     , WS.MonadWSLog m
     , MonadExecuteQuery m
     , Tracing.HasReporter m
     , MonadQueryInstrumentation m
     , MonadMetadataStorage m
     )
  => Env.Environment
  -> ServeOptions impl
  -> InitCtx
  -> Maybe PGSourceConfig
  -- ^ An optional specialized pg exection context for executing queries
  -- and mutations
  -> UTCTime
  -- ^ start time
  -> IO ()
  -- ^ shutdown function
  -> Maybe EL.LiveQueryPostPollHook
  -> EKG.Store
  -> SchemaSyncCtx
  -> m ()
runHGEServer env ServeOptions{..} InitCtx{..} _ initTime
  shutdownApp postPollHook ekgStore schemaSyncCtx = do
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
      Loggers loggerCtx logger _ = _icLoggers
      SchemaSyncCtx schemaSyncListenerThread schemaSyncEventRef = schemaSyncCtx

  authModeRes <- runExceptT $ setupAuthMode soAdminSecret soAuthHook soJwtSecret soUnAuthRole
                              _icHttpManager logger

  authMode <- either (printErrExit AuthConfigurationError . T.unpack) return authModeRes

  _idleGCThread <- C.forkImmortal "ourIdleGC" logger $ liftIO $
    ourIdleGC logger (seconds 0.3) (seconds 10) (seconds 60)

  HasuraApp app cacheRef stopWsServer <- flip onException (flushLogger loggerCtx) $
    mkWaiApp env
             logger
             sqlGenCtx
             soEnableAllowlist
             _icHttpManager
             authMode
             soCorsConfig
             soEnableConsole
             soConsoleAssetsDir
             soEnableTelemetry
             _icInstanceId
             soEnabledAPIs
             soLiveQueryOpts
             soPlanCacheOptions
             soResponseInternalErrorsConfig
             postPollHook
             _icSchemaCache
             ekgStore
             _icMetadataPool
             _icMetadata
             soConnectionOptions

  -- log inconsistent schema objects
  liftIO $ logInconsObjs logger $ scInconsistentObjs $ lastBuiltSchemaCache _icSchemaCache

  -- start background thread for schema sync event processing
  schemaSyncProcessorThread <-
    startSchemaSyncProcessorThread sqlGenCtx
    logger _icHttpManager schemaSyncEventRef cacheRef _icInstanceId

  let
    maxEvThrds    = fromMaybe defaultMaxEventThreads soEventsHttpPoolSize
    fetchI        = milliseconds $ fromMaybe (Milliseconds defaultFetchInterval) soEventsFetchInterval
    logEnvHeaders = soLogHeadersFromEnv
    allPgSources  = map _pcConfiguration $ HM.elems $ scPostgres $
                    lastBuiltSchemaCache _icSchemaCache

  lockedEventsCtx <- liftIO $ atomically initLockedEventsCtx

  eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEvThrds fetchI
  unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"

  eventQueueThread <- C.forkImmortal "processEventQueue" logger $
    processEventQueue logger logEnvHeaders
    _icHttpManager (getSCFromRef cacheRef) eventEngineCtx lockedEventsCtx

  -- start a backgroud thread to handle async actions
  asyncActionsThread <- C.forkImmortal "asyncActionsProcessor" logger $
    asyncActionsProcessor env logger (_scrCache cacheRef) _icHttpManager

  -- start a background thread to create new cron events
  cronEventsThread <- C.forkImmortal "runCronEventsGenerator" logger $
    runCronEventsGenerator logger (getSCFromRef cacheRef)

  -- prepare scheduled triggers
  prepareScheduledEvents logger

  -- start a background thread to deliver the scheduled events
  scheduledEventsThread <- C.forkImmortal "processScheduledTriggers" logger $
    processScheduledTriggers env logger logEnvHeaders _icHttpManager (getSCFromRef cacheRef) lockedEventsCtx

  -- start a background thread to check for updates
  updateThread <- C.forkImmortal "checkForUpdates" logger $ liftIO $
    checkForUpdates loggerCtx _icHttpManager

  -- start a background thread for telemetry
  telemetryThread <- if soEnableTelemetry
    then do
      unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice

      -- TODO: (anon) here we are getting the pg-version and db_uuid of the
      -- metadata database. And we are using it for telemetry. I think for
      -- telemetry we want these from the user's source databases, not the
      -- metadata db?
      pgVersion <- liftIO $
        runTxIO _icMetadataPool (Q.ReadCommitted, Nothing) $ getPgVersion

      dbUidE <- runMetadataStorageT getDatabaseUid
      dbUid <- either (printErrJExit MetadataDbUidFetchError) return dbUidE

      telemetryThread <- C.forkImmortal "runTelemetry" logger $ liftIO $
        runTelemetry logger _icHttpManager (getSCFromRef cacheRef) dbUid _icInstanceId pgVersion
      return $ Just telemetryThread
    else return Nothing

  -- all the immortal threads are collected so that they can be stopped when gracefully shutting down
  let immortalThreads = [ schemaSyncProcessorThread
                        , updateThread
                        , asyncActionsThread
                        , eventQueueThread
                        , scheduledEventsThread
                        , cronEventsThread
                        ] <> catMaybes [telemetryThread, schemaSyncListenerThread]

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  unLogger logger $
    mkGenericLog LevelInfo "server" $ StartupTimeInfo "starting API server" apiInitTime

  shutdownHandler' <- liftWithStateless $ \lowerIO ->
    pure $ shutdownHandler _icLoggers immortalThreads stopWsServer lockedEventsCtx allPgSources $
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
      either (printErrJExit EventSubSystemError) return res

    -- | shutdownEvents will be triggered when a graceful shutdown has been inititiated, it will
    -- get the locked events from the event engine context and the scheduled event engine context
    -- then it will unlock all those events.
    -- It may happen that an event may be processed more than one time, an event that has been already
    -- processed but not been marked as delivered in the db will be unlocked by `shutdownEvents`
    -- and will be processed when the events are proccessed next time.
    shutdownEvents
      :: [PGSourceConfig]
      -> (ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT IO Int)
      -> Logger Hasura
      -> LockedEventsCtx
      -> IO ()
    shutdownEvents pgSources unlockScheduledEvents' hasuraLogger@(Logger logger) LockedEventsCtx {..} = do
      forM pgSources $ \pgSource -> do
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

    -- runTx :: PGExecCtx -> Q.TxE QErr a -> IO (Either QErr a)
    -- runTx pgExecCtx tx =
    --   liftIO $ runExceptT $ runLazyTx pgExecCtx Q.ReadWrite $ liftTx tx

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
      -> [PGSourceConfig]
      -> (ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT IO Int)
      -> IO ()
      -- ^ the closeSocket callback
      -> IO ()
    shutdownHandler (Loggers loggerCtx (Logger logger) _) immortalThreads stopWsServer
                    leCtx pgSources unlockScheduledEvents' closeSocket =
      LA.link =<< LA.async do
        waitForShutdown _icShutdownLatch
        logger $ mkGenericStrLog LevelInfo "server" "gracefully shutting down server"
        shutdownEvents pgSources unlockScheduledEvents' (Logger logger) leCtx
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
               logger $ UnstructuredLog LevelWarn
                 "Overdue for a major GC: forcing one even though we don't appear to be idle"
             performMajorGC
             startTimer >>= go (gcs+1) (major_gcs+1)

         -- else keep the timer running, waiting for us to go idle:
         | otherwise -> do
             C.sleep idleInterval
             go gcs major_gcs timerSinceLastMajorGC

runAsAdmin
  :: (MonadIO m)
  => SQLGenCtx
  -> HTTP.Manager
  -> Metadata
  -> MetadataRun m a
  -> m (Either QErr a)
runAsAdmin sqlGenCtx httpManager metadata m =
  fmap (fmap fst) $ runExceptT $
    peelMetadataRun (RunCtx adminUserInfo httpManager sqlGenCtx) metadata m

execQuery
  :: ( HasVersion
     , MonadIO m
     )
  => Env.Environment
  -> HTTP.Manager
  -> Metadata
  -> BLC.ByteString
  -> m (Either QErr BLC.ByteString)
execQuery env httpManager metadata queryBs = runExceptT do
  QueryWithSource source query <- case A.decode queryBs of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"
  let runCtx = RunCtx adminUserInfo httpManager (SQLGenCtx False)
      actionM = do
        buildSchemaCacheStrict noMetadataModify
        encJToLBS <$> runQueryM env source query

  schemaCache <- buildRebuildableSchemaCache env
                 & peelMetadataRun runCtx metadata
                 & fmap fst

  let postgresSources = scPostgres $ lastBuiltSchemaCache schemaCache
  sourceConfig <- fmap _pcConfiguration $ onNothing (HM.lookup source postgresSources) $
                  throw400 NotExists $ "source " <> unSourceName source <> " does not exist"

  actionM & Tracing.runTraceTWithReporter Tracing.noReporter "execute"
          & runCacheRWT schemaCache
          & fmap (\(res, _, _) -> res)
          & peelQueryRun sourceConfig Q.ReadWrite Nothing runCtx metadata
          & fmap fst

instance Tracing.HasReporter ServerAppM

instance MonadQueryInstrumentation ServerAppM where
  askInstrumentQuery _ = pure (id, noProfile)

instance HttpLog ServerAppM where
  logHttpError logger userInfoM reqId waiReq req qErr headers =
    unLogger logger $ mkHttpLog $
      mkHttpErrorLogContext userInfoM reqId waiReq req qErr Nothing Nothing headers

  logHttpSuccess logger userInfoM reqId waiReq _reqBody _response compressedResponse qTime cType headers =
    unLogger logger $ mkHttpLog $
      mkHttpAccessLogContext userInfoM reqId waiReq compressedResponse qTime cType headers

instance MonadExecuteQuery ServerAppM where
  cacheLookup _ _ = pure ([], Nothing)
  cacheStore  _ _ = pure ()

instance UserAuthentication (Tracing.TraceT ServerAppM) where
  resolveUserInfo logger manager headers authMode =
    runExceptT $ getUserInfoWithExpTime logger manager headers authMode

accessDeniedErrMsg :: Text
accessDeniedErrMsg =
  "restricted access : admin only"

instance MonadApiAuthorization ServerAppM where
  authorizeMetadataApi _ userInfo = do
    let currRole = _uiRole userInfo
    when (currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

  authorizeQueryApi query userInfo = do
    let currRole = _uiRole userInfo
    when (queryNeedsAdmin query && currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

instance ConsoleRenderer ServerAppM where
  renderConsole path authMode enableTelemetry consoleAssetsDir =
    return $ mkConsoleHTML path authMode enableTelemetry consoleAssetsDir

instance MonadGQLExecutionCheck ServerAppM where
  checkGQLExecution userInfo _ enableAL sc query = runExceptT $ do
    req <- toParsed query
    checkQueryInAllowlist enableAL userInfo req sc
    return req

instance MonadConfigApiHandler ServerAppM where
  runConfigApiHandler = configApiGetHandler

instance MonadQueryLog ServerAppM where
  logQueryLog logger query genSqlM reqId =
    unLogger logger $ QueryLog query genSqlM reqId

instance WS.MonadWSLog ServerAppM where
  logWSLog = unLogger

runTxInMetadataStorage :: Q.TxE QErr a -> MetadataStorageT ServerAppM a
runTxInMetadataStorage tx = do
  pool <- lift ask
  liftEitherM $ liftIO $ runExceptT $
    Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) tx

notifySchemaCacheSyncTx :: InstanceId -> CacheInvalidations -> Q.TxE QErr ()
notifySchemaCacheSyncTx instanceId invalidations = do
  Q.Discard () <- Q.withQE defaultTxErrorHandler [Q.sql|
      SELECT pg_notify('hasura_schema_update', json_build_object(
        'instance_id', $1,
        'occurred_at', NOW(),
        'invalidations', $2
        )::text
      )
    |] (instanceId, Q.AltJ invalidations) True
  pure ()

processSchemaSyncEventPayload'
  :: (Monad m)
  => InstanceId -> A.Value -> m (Either Text SchemaSyncEventProcessResult)
processSchemaSyncEventPayload' instanceId payloadValue = pure $ do
  eventPayload <- fmapL qeError $ runExcept $ decodeValue payloadValue
  let _sseprShouldReload = instanceId /= _ssepInstanceId eventPayload
      _sseprCacheInvalidations = _ssepInvalidations eventPayload
  pure SchemaSyncEventProcessResult{..}

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


instance MonadMetadataStorage ServerAppM where

  getMetadata = runTxInMetadataStorage (fromMaybe emptyMetadata <$> fetchMetadataTx)
  setMetadata = runTxInMetadataStorage . setMetadataTx

  notifySchemaCacheSync a b = runTxInMetadataStorage $ notifySchemaCacheSyncTx a b
  processSchemaSyncEventPayload instanceId payloadValue = do
    eventPayload <- decodeValue payloadValue
    let _sseprShouldReload = instanceId /= _ssepInstanceId eventPayload
        _sseprCacheInvalidations = _ssepInvalidations eventPayload
    pure SchemaSyncEventProcessResult{..}

  getCatalogState     = runTxInMetadataStorage getCatalogStateTx
  setCatalogState a b = runTxInMetadataStorage $ setCatalogStateTx a b
  getDatabaseUid      = runTxInMetadataStorage getDbIdTx

  insertAction a b c d         = runTxInMetadataStorage $ insertActionTx a b c d
  fetchUndeliveredActionEvents = runTxInMetadataStorage undeliveredEventsTx
  setActionStatus a b          = runTxInMetadataStorage $ setActionStatusTx a b
  fetchActionResponse          = runTxInMetadataStorage . fetchActionResponseTx

  getDeprivedCronTriggerStats        = runTxInMetadataStorage getDeprivedCronTriggerStatsTx
  getScheduledEventsForDelivery      = runTxInMetadataStorage getScheduledEventsForDeliveryTx
  getOneOffScheduledEvents a b       = runTxInMetadataStorage $ getOneOffScheduledEventsTx a b
  getCronEvents a b c                = runTxInMetadataStorage $ getCronEventsTx a b c
  getInvocations a b                 = runTxInMetadataStorage $ getInvocationsTx a b
  insertScheduledEvent               = runTxInMetadataStorage . insertScheduledEventTx
  insertScheduledEventInvocation a b = runTxInMetadataStorage $ insertInvocationTx a b
  setScheduledEventOp a b c          = runTxInMetadataStorage $ setScheduledEventOpTx a b c
  unlockScheduledEvents a b          = runTxInMetadataStorage $ unlockScheduledEventsTx a b
  unlockAllLockedScheduledEvents     = runTxInMetadataStorage unlockAllLockedScheduledEventsTx
  clearFutureCronEvents              = runTxInMetadataStorage . clearFutureCronEventsTx
  deleteScheduledEvent a b           = runTxInMetadataStorage $ deleteScheduledEventTx a b

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
