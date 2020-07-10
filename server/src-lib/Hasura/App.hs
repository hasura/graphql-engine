{-# LANGUAGE UndecidableInstances #-}

module Hasura.App where

import           Control.Concurrent.STM.TVar               (readTVarIO,TVar)
import           Control.Lens                              (view, _2)
import           Control.Monad.Base
import           Control.Monad.Catch                       (MonadCatch, MonadThrow, onException)
import           Control.Monad.Stateless
import           Control.Monad.STM                         (atomically)
import           Control.Monad.Trans.Control               (MonadBaseControl (..))
import           Control.Monad.Unique
import           Data.Aeson                                ((.=))
import           Data.Time.Clock                           (UTCTime)
import           GHC.AssertNF
import           Options.Applicative
import           System.Environment                        (getEnvironment, lookupEnv)
import           System.Exit                               (exitFailure)

import qualified Control.Concurrent.Async                  as Async
import qualified Control.Concurrent.Async.Lifted.Safe      as LA
import qualified Control.Concurrent.Extended               as C
import qualified Data.Aeson                                as A
import qualified Data.ByteString.Char8                     as BC
import qualified Data.ByteString.Lazy.Char8                as BLC
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

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Eventing.EventTrigger
import           Hasura.Eventing.Common
import           Hasura.Eventing.ScheduledTrigger
import           Hasura.GraphQL.Execute                    (MonadGQLExecutionCheck (..),
                                                            -- checkQueryInAllowlist
                                                           )
import           Hasura.GraphQL.Logging                    (MonadQueryLog (..), QueryLog (..))
import           Hasura.GraphQL.Resolve.Action             (asyncActionsProcessor)
import           Hasura.GraphQL.Transport.HTTP.Protocol    (toParsed)
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.Types                          (CacheRWM, Code (..), HasHttpManager,
                                                            HasSQLGenCtx, HasSystemDefined,
                                                            QErr (..), SQLGenCtx (..),
                                                            SchemaCache (..), UserInfoM,
                                                            buildSchemaCacheStrict, decodeValue,
                                                            throw400, withPathK)
import           Hasura.RQL.Types.Run
import           Hasura.Server.API.Query                   (fetchLastUpdate, requiresAdmin,
                                                            runQueryM)
import           Hasura.Server.App
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates                (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Migrate                     (migrateCatalog)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Version
import           Hasura.Session

import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS

printErrExit :: (MonadIO m) => forall a . String -> m a
printErrExit = liftIO . (>> exitFailure) . putStrLn

printErrJExit :: (A.ToJSON a, MonadIO m) => forall b . a -> m b
printErrJExit = liftIO . (>> exitFailure) . printJSON

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
  either printErrExit return eitherOpts
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


-- | most of the required types for initializing graphql-engine
data InitCtx
  = InitCtx
  { _icHttpManager   :: !HTTP.Manager
  , _icInstanceId    :: !InstanceId
  , _icLoggers       :: !Loggers
  , _icConnInfo      :: !Q.ConnInfo
  , _icPgPool        :: !Q.PGPool
  , _icShutdownLatch :: !ShutdownLatch
  , _icSchemaCache   :: !(RebuildableSchemaCache Run, Maybe UTCTime)
  }

-- | Collection of the LoggerCtx, the regular Logger and the PGLogger
-- TODO: better naming?
data Loggers
  = Loggers
  { _lsLoggerCtx :: !(LoggerCtx Hasura)
  , _lsLogger    :: !(Logger Hasura)
  , _lsPgLogger  :: !Q.PGLogger
  }

newtype AppM a = AppM { unAppM :: IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadUnique, MonadBase IO
           , MonadBaseControl IO, MonadCatch, MonadThrow)

-- | this function initializes the catalog and returns an @InitCtx@, based on the command given
-- - for serve command it creates a proper PG connection pool
-- - for other commands, it creates a minimal pool
-- this exists as a separate function because the context (logger, http manager, pg pool) can be
-- used by other functions as well
initialiseCtx
  :: (HasVersion, MonadIO m, MonadCatch m)
  => HGECommand Hasura
  -> RawConnInfo
  -> m (InitCtx, UTCTime)
initialiseCtx hgeCmd rci = do
  initTime <- liftIO Clock.getCurrentTime
  -- global http manager
  httpManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  instanceId <- liftIO generateInstanceId
  connInfo <- liftIO procConnInfo
  latch <- liftIO newShutdownLatch
  (loggers, pool, sqlGenCtx) <- case hgeCmd of
    -- for the @serve@ command generate a regular PG pool
    HCServe so@ServeOptions{..} -> do
      l@(Loggers _ logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel
      -- log serve options
      unLogger logger $ serveOptsToLog so
      -- log postgres connection info
      unLogger logger $ connInfoToLog connInfo
      pool <- liftIO $ Q.initPGPool connInfo soConnParams pgLogger
      pure (l, pool, SQLGenCtx soStringifyNum)

    -- for other commands generate a minimal PG pool
    _ -> do
      l@(Loggers _ _ pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      pool <- getMinimalPool pgLogger connInfo
      pure (l, pool, SQLGenCtx False)

  res <- flip onException (flushLogger (_lsLoggerCtx loggers)) $
    migrateCatalogSchema (_lsLogger loggers) pool httpManager sqlGenCtx
  pure (InitCtx httpManager instanceId loggers connInfo pool latch res, initTime)
  where
    procConnInfo =
      either (printErrExit . ("Fatal Error : " <>)) return $ mkConnInfo rci

    getMinimalPool pgLogger ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      liftIO $ Q.initPGPool ci connParams pgLogger

    mkLoggers enabledLogs logLevel = do
      loggerCtx <- liftIO $ mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
      let logger = mkLogger loggerCtx
          pgLogger = mkPGLogger logger
      return $ Loggers loggerCtx logger pgLogger

-- | helper function to initialize or migrate the @hdb_catalog@ schema (used by pro as well)
migrateCatalogSchema
  :: (HasVersion, MonadIO m)
  => Logger Hasura -> Q.PGPool -> HTTP.Manager -> SQLGenCtx
  -> m (RebuildableSchemaCache Run, Maybe UTCTime)
migrateCatalogSchema logger pool httpManager sqlGenCtx = do
  let pgExecCtx = mkPGExecCtx Q.Serializable pool
      adminRunCtx = RunCtx adminUserInfo httpManager sqlGenCtx
  currentTime <- liftIO Clock.getCurrentTime
  initialiseResult <- runExceptT $ peelRun adminRunCtx pgExecCtx Q.ReadWrite $
    (,) <$> migrateCatalog currentTime <*> liftTx fetchLastUpdate

  ((migrationResult, schemaCache), lastUpdateEvent) <-
    initialiseResult `onLeft` \err -> do
      unLogger logger StartupLog
        { slLogLevel = LevelError
        , slKind = "db_migrate"
        , slInfo = A.toJSON err
        }
      liftIO exitFailure
  unLogger logger migrationResult
  return (schemaCache, view _2 <$> lastUpdateEvent)

-- | Run a transaction and if an error is encountered, log the error and abort the program
runTxIO :: Q.PGPool -> Q.TxMode -> Q.TxE QErr a -> IO a
runTxIO pool isoLevel tx = do
  eVal <- liftIO $ runExceptT $ Q.runTx pool isoLevel tx
  either printErrJExit return eVal

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
shutdownGracefully = flip C.putMVar () . unShutdownLatch . _icShutdownLatch

-- | If an exception is encountered , flush the log buffer and
-- rethrow If we do not flush the log buffer on exception, then log lines
-- may be missed
-- See: https://github.com/hasura/graphql-engine/issues/4772
flushLogger :: (MonadIO m) => LoggerCtx impl -> m ()
flushLogger loggerCtx = liftIO $ FL.flushLogStr $ _lcLoggerSet loggerCtx

runHGEServer
  :: ( HasVersion
     , MonadIO m
     , MonadUnique m
     , MonadCatch m
     , MonadStateless IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication m
     , MetadataApiAuthorization m
     , HttpLog m
     , MonadQueryLog m
     , ConsoleRenderer m
     , MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , WS.MonadWSLog m
     )
  => ServeOptions impl
  -> InitCtx
  -> Maybe PGExecCtx
  -- ^ An optional specialized pg exection context for executing queries
  -- and mutations
  -> UTCTime
  -- ^ start time
  -> m ()
runHGEServer ServeOptions{..} InitCtx{..} pgExecCtx initTime = do
  -- Comment this to enable expensive assertions from "GHC.AssertNF". These
  -- will log lines to STDOUT containing "not in normal form". In the future we
  -- could try to integrate this into our tests. For now this is a development
  -- tool.
  --
  -- NOTE: be sure to compile WITHOUT code coverage, for this to work properly.
  liftIO disableAssertNF

  let sqlGenCtx = SQLGenCtx soStringifyNum
      Loggers loggerCtx logger _ = _icLoggers

  authModeRes <- runExceptT $ setupAuthMode soAdminSecret soAuthHook soJwtSecret soUnAuthRole
                              _icHttpManager logger

  authMode <- either (printErrExit . T.unpack) return authModeRes

  HasuraApp app cacheRef cacheInitTime shutdownApp <- flip onException (flushLogger loggerCtx) $
    mkWaiApp soTxIso
             logger
             sqlGenCtx
             soEnableAllowlist
             _icPgPool
             pgExecCtx
             _icConnInfo
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
             _icSchemaCache

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSCFromRef cacheRef)
  liftIO $ logInconsObjs logger inconsObjs

  -- start background threads for schema sync
  (_schemaSyncListenerThread, _schemaSyncProcessorThread) <-
    startSchemaSyncThreads sqlGenCtx _icPgPool logger _icHttpManager
                           cacheRef _icInstanceId cacheInitTime

  maxEvThrds <- liftIO $ getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
  fetchI  <- fmap milliseconds $ liftIO $
    getFromEnv (Milliseconds defaultFetchInterval) "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
  logEnvHeaders <- liftIO $ getFromEnv False "LOG_HEADERS_FROM_ENV"

  lockedEventsCtx <- liftIO $ atomically $ initLockedEventsCtx

  -- prepare event triggers data
  prepareEvents _icPgPool logger
  eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEvThrds fetchI
  unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"
  _eventQueueThread <- C.forkImmortal "processEventQueue" logger $ liftIO $
    processEventQueue logger logEnvHeaders
    _icHttpManager _icPgPool (getSCFromRef cacheRef) eventEngineCtx lockedEventsCtx

  -- start a backgroud thread to handle async actions
  _asyncActionsThread <- C.forkImmortal "asyncActionsProcessor" logger $ liftIO $
    asyncActionsProcessor (_scrCache cacheRef) _icPgPool _icHttpManager

  -- start a background thread to create new cron events
  void $ liftIO $ C.forkImmortal "runCronEventsGenerator" logger $
    runCronEventsGenerator logger _icPgPool (getSCFromRef cacheRef)

  -- prepare scheduled triggers
  prepareScheduledEvents _icPgPool logger

  -- start a background thread to deliver the scheduled events
  void $ liftIO $ C.forkImmortal "processScheduledTriggers" logger  $ processScheduledTriggers logger logEnvHeaders _icHttpManager _icPgPool (getSCFromRef cacheRef) lockedEventsCtx

  -- start a background thread to check for updates
  _updateThread <- C.forkImmortal "checkForUpdates" logger $ liftIO $
    checkForUpdates loggerCtx _icHttpManager

  -- start a background thread for telemetry
  when soEnableTelemetry $ do
    unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice
    (dbId, pgVersion) <- liftIO $ runTxIO _icPgPool (Q.ReadCommitted, Nothing) $
      (,) <$> getDbId <*> getPgVersion
    void $ C.forkImmortal "runTelemetry" logger $ liftIO $
      runTelemetry logger _icHttpManager (getSCFromRef cacheRef) dbId _icInstanceId pgVersion

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  unLogger logger $
    mkGenericLog LevelInfo "server" $ StartupTimeInfo "starting API server" apiInitTime
  let warpSettings = Warp.setPort soPort
                     . Warp.setHost soHost
                     . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
                     . Warp.setInstallShutdownHandler (shutdownHandler _icLoggers shutdownApp lockedEventsCtx _icPgPool)
                     $ Warp.defaultSettings
  liftIO $ Warp.runSettings warpSettings app

  where
    -- | prepareEvents is a function to unlock all the events that are
    -- locked and unprocessed, which is called while hasura is started.
    -- Locked and unprocessed events can occur in 2 ways
    -- 1.
    -- Hasura's shutdown was not graceful in which all the fetched
    -- events will remain locked and unprocessed(TODO: clean shutdown)
    -- state.
    -- 2.
    -- There is another hasura instance which is processing events and
    -- it will lock events to process them.
    -- So, unlocking all the locked events might re-deliver an event(due to #2).
    prepareEvents pool (Logger logger) = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "event_triggers" "preparing data"
      res <- liftIO $ runTx pool (Q.ReadCommitted, Nothing) unlockAllEvents
      either printErrJExit return res

    -- | prepareScheduledEvents is like prepareEvents, but for scheduled triggers
    prepareScheduledEvents pool (Logger logger) = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "scheduled_triggers" "preparing data"
      res <- liftIO $ runTx pool (Q.ReadCommitted, Nothing) unlockAllLockedScheduledEvents
      either printErrJExit return res

    -- | shutdownEvents will be triggered when a graceful shutdown has been inititiated, it will
    -- get the locked events from the event engine context and the scheduled event engine context
    -- then it will unlock all those events.
    -- It may happen that an event may be processed more than one time, an event that has been already
    -- processed but not been marked as delivered in the db will be unlocked by `shutdownEvents`
    -- and will be processed when the events are proccessed next time.
    shutdownEvents
      :: Q.PGPool
      -> Logger Hasura
      -> LockedEventsCtx
      -> IO ()
    shutdownEvents pool hasuraLogger@(Logger logger) LockedEventsCtx {..} = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "event_triggers" "unlocking events that are locked by the HGE"
      unlockEventsForShutdown pool hasuraLogger "event_triggers" "" unlockEvents leEvents
      liftIO $ logger $ mkGenericStrLog LevelInfo "scheduled_triggers" "unlocking scheduled events that are locked by the HGE"
      unlockEventsForShutdown pool hasuraLogger "scheduled_triggers" "cron events" unlockCronEvents leCronEvents
      unlockEventsForShutdown pool hasuraLogger "scheduled_triggers" "scheduled events" unlockCronEvents leStandAloneEvents

    unlockEventsForShutdown
      :: Q.PGPool
      -> Logger Hasura
      -> Text -- ^ trigger type
      -> Text -- ^ event type
      -> ([eventId] -> Q.TxE QErr Int)
      -> TVar (Set.Set eventId)
      -> IO ()
    unlockEventsForShutdown pool (Logger logger) triggerType eventType doUnlock lockedIdsVar = do
      lockedIds <- readTVarIO lockedIdsVar
      unless (Set.null lockedIds) do
        result <- runTx pool (Q.ReadCommitted, Nothing) (doUnlock $ toList lockedIds)
        case result of
          Left err -> logger $ mkGenericStrLog LevelWarn triggerType $
            "Error while unlocking " ++ T.unpack eventType ++ " events: " ++ show err
          Right count -> logger $ mkGenericStrLog LevelInfo triggerType $
            show count ++ " " ++ T.unpack eventType ++ " events successfully unlocked"

    getFromEnv :: (Read a) => a -> String -> IO a
    getFromEnv defaults env = do
      mEnv <- lookupEnv env
      let mRes = case mEnv of
            Nothing  -> Just defaults
            Just val -> readMaybe val
          eRes = maybe (Left $ "Wrong expected type for environment variable: " <> env) Right mRes
      either printErrExit return eRes

    runTx :: Q.PGPool -> Q.TxMode -> Q.TxE QErr a -> IO (Either QErr a)
    runTx pool txLevel tx =
      liftIO $ runExceptT $ Q.runTx pool txLevel tx

    -- | Waits for the shutdown latch 'MVar' to be filled, and then
    -- shuts down the server and associated resources.
    -- Structuring things this way lets us decide elsewhere exactly how
    -- we want to control shutdown.
    shutdownHandler
      :: Loggers
      -> IO ()
      -> LockedEventsCtx
      -> Q.PGPool
      -> IO ()
      -> IO ()
    shutdownHandler (Loggers loggerCtx (Logger logger) _) shutdownApp leCtx pool closeSocket =
      void . Async.async $ do
        waitForShutdown _icShutdownLatch
        logger $ mkGenericStrLog LevelInfo "server" "gracefully shutting down server"
        shutdownEvents pool (Logger logger) leCtx
        closeSocket
        shutdownApp
        cleanLoggerCtx loggerCtx

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
  runExceptT $ peelRun runCtx pgCtx Q.ReadWrite m

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
     )
  => BLC.ByteString
  -> m BLC.ByteString
execQuery queryBs = do
  query <- case A.decode queryBs of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"
  buildSchemaCacheStrict
  encJToLBS <$> runQueryM query


instance HttpLog AppM where
  logHttpError logger userInfoM reqId httpReq req qErr headers =
    unLogger logger $ mkHttpLog $
      mkHttpErrorLogContext userInfoM reqId httpReq qErr req Nothing Nothing headers

  logHttpSuccess logger userInfoM reqId httpReq _ _ compressedResponse qTime cType headers =
    unLogger logger $ mkHttpLog $
      mkHttpAccessLogContext userInfoM reqId httpReq compressedResponse qTime cType headers

instance UserAuthentication AppM where
  resolveUserInfo logger manager headers authMode =
    runExceptT $ getUserInfoWithExpTime logger manager headers authMode

instance MetadataApiAuthorization AppM where
  authorizeMetadataApi query userInfo = do
    let currRole = _uiRole userInfo
    when (requiresAdmin query && currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied errMsg
    where
      errMsg = "restricted access : admin only"

instance ConsoleRenderer AppM where
  renderConsole path authMode enableTelemetry consoleAssetsDir =
    return $ mkConsoleHTML path authMode enableTelemetry consoleAssetsDir

instance MonadGQLExecutionCheck AppM where
  checkGQLExecution userInfo _ enableAL sc query = runExceptT $ do
    req <- toParsed query

    -- TODO FIXME

    -- The following check is currently done in Execute.hs.  This code is
    -- commented out in the PDV branch because in the PDV refactor we are so far
    -- not using MonadGQLExecutionCheck yet. So we should figure out what
    -- happened on master, and correspondingly re-enable this check here.

    -- checkQueryInAllowlist enableAL userInfo req sc
    return req

instance MonadConfigApiHandler AppM where
  runConfigApiHandler = configApiGetHandler

instance MonadQueryLog AppM where
  logQueryLog logger query genSqlM reqId =
    unLogger logger $ QueryLog query genSqlM reqId

instance WS.MonadWSLog AppM where
  logWSLog = unLogger


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
