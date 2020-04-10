{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.App where

import           Control.Monad.Base
import           Control.Monad.Stateless
import           Control.Monad.STM                    (atomically)
import           Control.Monad.Trans.Control          (MonadBaseControl (..))
import           Data.Aeson                           ((.=))
import           Data.Time.Clock                      (UTCTime, getCurrentTime)
import           GHC.AssertNF
import           Options.Applicative
import           System.Environment                   (getEnvironment, lookupEnv)
import           System.Exit                          (exitFailure)

import qualified Control.Concurrent.Extended          as C
import qualified Control.Concurrent.Async.Lifted.Safe as LA
import qualified Data.Aeson                           as A
import qualified Data.ByteString.Char8                as BC
import qualified Data.ByteString.Lazy.Char8           as BLC
import qualified Data.Text                            as T
import qualified Data.Time.Clock                      as Clock
import qualified Data.Yaml                            as Y
import qualified Database.PG.Query                    as Q
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Client.TLS              as HTTP
import qualified Network.Wai.Handler.Warp             as Warp
import qualified System.Posix.Signals                 as Signals
import qualified Text.Mustache.Compile                as M

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Events.Lib
import           Hasura.GraphQL.Resolve.Action        (asyncActionsProcessor)
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types                     (CacheRWM, Code (..), HasHttpManager,
                                                       HasSQLGenCtx, HasSystemDefined, QErr (..),
                                                       SQLGenCtx (..), SchemaCache (..), UserInfoM,
                                                       adminRole, adminUserInfo,
                                                       buildSchemaCacheStrict, decodeValue,
                                                       throw400, userRole, withPathK)
import           Hasura.RQL.Types.Run
import           Hasura.Server.App
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates           (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Migrate                (migrateCatalog)
import           Hasura.Server.Query                  (requiresAdmin, runQueryM)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Version


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
  { _icHttpManager :: !HTTP.Manager
  , _icInstanceId  :: !InstanceId
  , _icDbUid       :: !Text
  , _icLoggers     :: !Loggers
  , _icConnInfo    :: !Q.ConnInfo
  , _icPgPool      :: !Q.PGPool
  , _icPgVersion   :: !PGVersion
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
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO)

-- | this function initializes the catalog and returns an @InitCtx@, based on the command given
-- - for serve command it creates a proper PG connection pool
-- - for other commands, it creates a minimal pool
-- this exists as a separate function because the context (logger, http manager, pg pool) can be
-- used by other functions as well
initialiseCtx
  :: (HasVersion, MonadIO m)
  => HGECommand Hasura
  -> RawConnInfo
  -> m (InitCtx, UTCTime)
initialiseCtx hgeCmd rci = do
  initTime <- liftIO Clock.getCurrentTime
  -- global http manager
  httpManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  instanceId <- liftIO generateInstanceId
  connInfo <- liftIO procConnInfo
  (loggers, pool) <- case hgeCmd of
    -- for server command generate a proper pool
    HCServe so@ServeOptions{..} -> do
      l@(Loggers _ logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel
      -- log serve options
      unLogger logger $ serveOptsToLog so
      -- log postgres connection info
      unLogger logger $ connInfoToLog connInfo
      pool <- liftIO $ Q.initPGPool connInfo soConnParams pgLogger
      -- safe init catalog
      initialiseCatalog pool (SQLGenCtx soStringifyNum) httpManager logger

      return (l, pool)

    -- for other commands generate a minimal pool
    _ -> do
      l@(Loggers _ _ pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      pool <- getMinimalPool pgLogger connInfo
      return (l, pool)

  -- get the unique db id
  dbId <- liftIO $ runTxIO pool (Q.Serializable, Nothing) getDbId

  -- get the pg version
  pgVersion <- liftIO $ runTxIO pool (Q.ReadCommitted, Nothing) getPgVersion

  return (InitCtx httpManager instanceId dbId loggers connInfo pool pgVersion, initTime)

  where
    procConnInfo =
      either (printErrExit . connInfoErrModifier) return $ mkConnInfo rci

    getMinimalPool pgLogger ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      liftIO $ Q.initPGPool ci connParams pgLogger

    mkLoggers enabledLogs logLevel = do
      loggerCtx <- liftIO $ mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
      let logger = mkLogger loggerCtx
          pgLogger = mkPGLogger logger
      return $ Loggers loggerCtx logger pgLogger

    initialiseCatalog pool sqlGenCtx httpManager (Logger logger) = do
      currentTime <- liftIO getCurrentTime
      -- initialise the catalog
      initRes <- runAsAdmin pool sqlGenCtx httpManager $ migrateCatalog currentTime
      either printErrJExit (\(result, schemaCache) -> logger result $> schemaCache) initRes

-- | Run a transaction and if an error is encountered, log the error and abort the program
runTxIO :: Q.PGPool -> Q.TxMode -> Q.TxE QErr a -> IO a
runTxIO pool isoLevel tx = do
  eVal <- liftIO $ runExceptT $ Q.runTx pool isoLevel tx
  either printErrJExit return eVal

runHGEServer
  :: ( HasVersion
     , MonadIO m
     , MonadStateless IO m
     , UserAuthentication m
     , MetadataApiAuthorization m
     , HttpLog m
     , ConsoleRenderer m
     , LA.Forall (LA.Pure m)
     )
  => ServeOptions impl
  -> InitCtx
  -> UTCTime
  -- ^ start time
  -> m ()
runHGEServer ServeOptions{..} InitCtx{..} initTime = do
  -- Comment this to enable expensive assertions from "GHC.AssertNF". These will log lines to
  -- STDOUT containing "not in normal form". In the future we could try to integrate this into
  -- our tests. For now this is a development tool.
  --
  -- NOTE: be sure to compile WITHOUT code coverage, for this to work properly.
  liftIO disableAssertNF

  let sqlGenCtx = SQLGenCtx soStringifyNum
      Loggers loggerCtx logger _ = _icLoggers

  authModeRes <- runExceptT $ mkAuthMode soAdminSecret soAuthHook soJwtSecret soUnAuthRole
                              _icHttpManager logger

  authMode <- either (printErrExit . T.unpack) return authModeRes

  HasuraApp app cacheRef cacheInitTime shutdownApp <- mkWaiApp soTxIso
                                                               logger
                                                               sqlGenCtx
                                                               soEnableAllowlist
                                                               _icPgPool
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

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSCFromRef cacheRef)
  liftIO $ logInconsObjs logger inconsObjs

  -- start background threads for schema sync
  (_schemaSyncListenerThread, _schemaSyncProcessorThread) <-
    startSchemaSyncThreads sqlGenCtx _icPgPool logger _icHttpManager
                           cacheRef _icInstanceId cacheInitTime

  let warpSettings = Warp.setPort soPort
                     . Warp.setHost soHost
                     . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
                     . Warp.setInstallShutdownHandler (shutdownHandler logger shutdownApp)
                     $ Warp.defaultSettings

  maxEvThrds <- liftIO $ getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
  fetchI  <- fmap milliseconds $ liftIO $
    getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
  logEnvHeaders <- liftIO $ getFromEnv False "LOG_HEADERS_FROM_ENV"

  -- prepare event triggers data
  prepareEvents _icPgPool logger
  eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEvThrds fetchI
  unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"
  _eventQueueThread <- C.forkImmortal "processEventQueue" logger $ liftIO $
    processEventQueue logger logEnvHeaders
    _icHttpManager _icPgPool (getSCFromRef cacheRef) eventEngineCtx

  -- start a backgroud thread to handle async actions
  _asyncActionsThread <- C.forkImmortal "asyncActionsProcessor" logger $ liftIO $
    asyncActionsProcessor (_scrCache cacheRef) _icPgPool _icHttpManager

  -- start a background thread to check for updates
  _updateThread <- C.forkImmortal "checkForUpdates" logger $ liftIO $
    checkForUpdates loggerCtx _icHttpManager

  -- start a background thread for telemetry
  when soEnableTelemetry $ do
    unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice
    void $ C.forkImmortal "runTelemetry" logger $ liftIO $
      runTelemetry logger _icHttpManager (getSCFromRef cacheRef) _icDbUid _icInstanceId _icPgVersion

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  unLogger logger $
    mkGenericLog LevelInfo "server" $ StartupTimeInfo "starting API server" apiInitTime
  liftIO $ Warp.runSettings warpSettings app

  where
    prepareEvents pool (Logger logger) = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "event_triggers" "preparing data"
      res <- runTx pool unlockAllEvents
      either printErrJExit return res

    getFromEnv :: (Read a) => a -> String -> IO a
    getFromEnv defaults env = do
      mEnv <- lookupEnv env
      let mRes = case mEnv of
            Nothing  -> Just defaults
            Just val -> readMaybe val
          eRes = maybe (Left $ "Wrong expected type for environment variable: " <> env) Right mRes
      either printErrExit return eRes

    runTx pool tx =
      liftIO $ runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx

    -- | Catches the SIGTERM signal and initiates a graceful shutdown. Graceful shutdown for regular HTTP
    -- requests is already implemented in Warp, and is triggered by invoking the 'closeSocket' callback.
    -- We only catch the SIGTERM signal once, that is, if the user hits CTRL-C once again, we terminate
    -- the process immediately.
    shutdownHandler :: Logger Hasura -> IO () -> IO () -> IO ()
    shutdownHandler (Logger logger) shutdownApp closeSocket =
      void $ Signals.installHandler
        Signals.sigTERM
        (Signals.CatchOnce shutdownSequence)
        Nothing
     where
      shutdownSequence = do
        closeSocket
        shutdownApp
        logShutdown

      logShutdown = logger $ mkGenericStrLog LevelInfo "server" "gracefully shutting down server"

runAsAdmin
  :: (MonadIO m)
  => Q.PGPool
  -> SQLGenCtx
  -> HTTP.Manager
  -> Run a
  -> m (Either QErr a)
runAsAdmin pool sqlGenCtx httpManager m = do
  let runCtx = RunCtx adminUserInfo httpManager sqlGenCtx
      pgCtx = PGExecCtx pool Q.Serializable
  runExceptT $ peelRun runCtx pgCtx Q.ReadWrite m

execQuery
  :: ( HasVersion
     , CacheRWM m
     , MonadTx m
     , MonadIO m
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
    let currRole = userRole userInfo
    when (requiresAdmin query && currRole /= adminRole) $
      withPathK "args" $ throw400 AccessDenied errMsg
    where
      errMsg = "restricted access : admin only"

instance ConsoleRenderer AppM where
  renderConsole path authMode enableTelemetry consoleAssetsDir =
    return $ mkConsoleHTML path authMode enableTelemetry consoleAssetsDir

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
