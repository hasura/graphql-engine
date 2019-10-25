{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Hasura.App where

import           Control.Monad.STM          (atomically)
import           Data.Time.Clock            (getCurrentTime)
import           Options.Applicative
import           System.Environment         (getEnvironment, lookupEnv)
import           System.Exit                (exitFailure)


import qualified Control.Concurrent         as C
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import qualified Data.Time.Clock            as Clock
import qualified Data.Yaml                  as Y
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import qualified Network.HTTP.Types         as HTTP
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.Posix.Signals       as Signals
import qualified Web.Spock.Core             as Spock

import           Hasura.App.Migrate         (migrateCatalog)
import           Hasura.App.Ops
import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Events.Lib
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types           (QErr (..), SQLGenCtx (..),
                                             SchemaCache (..), UserInfo,
                                             adminUserInfo, emptySchemaCache,
                                             isAdmin, userRole)
import           Hasura.Server.App
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Context
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Query        (RQLQuery, Run, peelRun)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Utils

import qualified Database.PG.Query          as Q


printErrExit :: forall a . String -> IO a
printErrExit = (>> exitFailure) . putStrLn

printErrJExit :: A.ToJSON a => forall b . a -> IO b
printErrJExit = (>> exitFailure) . printJSON

parseHGECommand :: Parser RawHGECommand
parseHGECommand =
  subparser
    ( command "serve" (info (helper <*> (HCServe <$> serveOpts))
          ( progDesc "Start the GraphQL Engine Server"
            <> footerDoc (Just serveCmdFooter)
          ))
        <> command "export" (info (pure  HCExport)
          ( progDesc "Export graphql-engine's metadata to stdout" ))
        <> command "clean" (info (pure  HCClean)
          ( progDesc "Clean graphql-engine's metadata to start afresh" ))
        <> command "execute" (info (pure  HCExecute)
          ( progDesc "Execute a query" ))
        <> command "version" (info (pure  HCVersion)
          (progDesc "Prints the version of GraphQL Engine"))
    )
  where
    serveOpts = RawServeOptions
                <$> parseServerPort
                <*> parseServerHost
                <*> parseConnParams
                <*> parseTxIsolation
                <*> (parseAdminSecret <|> parseAccessKey)
                <*> parseWebHook
                <*> parseJwtSecret
                <*> parseUnAuthRole
                <*> parseCorsConfig
                <*> parseEnableConsole
                <*> parseConsoleAssetsDir
                <*> parseEnableTelemetry
                <*> parseWsReadCookie
                <*> parseStringifyNum
                <*> parseEnabledAPIs
                <*> parseMxRefetchInt
                <*> parseMxBatchSize
                <*> parseFallbackRefetchInt
                <*> parseEnableAllowlist
                <*> parseEnabledLogs
                <*> parseLogLevel

parseArgs :: IO HGEOptions
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

printJSON :: (A.ToJSON a) => a -> IO ()
printJSON = BLC.putStrLn . A.encode

printYaml :: (A.ToJSON a) => a -> IO ()
printYaml = BC.putStrLn . Y.encode

mkPGLogger :: Logger -> Q.PGLogger
mkPGLogger (Logger logger) (Q.PLERetryMsg msg) =
  logger $ PGLog LevelWarn msg


-- | most of the required types for initializing graphql-engine
data InitCtx a
  = InitCtx
  { _icHttpManager :: !HTTP.Manager
  , _icInstanceId  :: !InstanceId
  , _icDbUid       :: !Text
  , _icLoggers     :: !(Loggers a)
  , _icConnInfo    :: !Q.ConnInfo
  , _icPgPool      :: !Q.PGPool
  }

-- | Collection of the LoggerCtx, the regular Logger and the PGLogger, and HttpLogger
-- TODO: better naming?
data Loggers a
  = Loggers
  { _lsLoggerCtx  :: !LoggerCtx
  , _lsLogger     :: !Logger
  , _lsPgLogger   :: !Q.PGLogger
  , _lsHttpLogger :: !(HttpLogger a)
  , _lsRespLogger :: !(Maybe HttpResponseLogger)
  }

newtype AppM a = AppM { unAppM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance HasuraSpockAction AppM where
  makeSpockAction serverCtx httpLogger qErrEncoder qErrModifier apiHandler = do
    req <- Spock.request
    reqBody <- liftIO $ Wai.strictRequestBody req
    let headers = Wai.requestHeaders req
        authMode = scAuthMode serverCtx
        manager = scManager serverCtx

    requestId <- getRequestId headers

    -- default to @getUserInfo@ if no user-auth middleware is passed
    --let resolveUserInfo = fromMaybe getUserInfo userAuthMiddleware
    userInfoE <- liftIO $ runExceptT $ getUserInfo logger manager headers authMode
    userInfo  <- either (logErrorAndResp Nothing requestId req (Left reqBody) False headers . qErrModifier)
                return userInfoE

    let handlerState = HandlerCtx serverCtx userInfo headers requestId
        curRole = userRole userInfo

    t1 <- liftIO getCurrentTime -- for measuring response time purposes

    (result, q) <- case apiHandler of
      AHGet handler -> do
        res <- liftIO $ runReaderT (runExceptT handler) handlerState
        return (res, Nothing)
      AHPost handler -> do
        parsedReqE <- runExceptT $ parseBody reqBody
        parsedReq  <- either (logErrorAndResp (Just userInfo) requestId req (Left reqBody) (isAdmin curRole) headers . qErrModifier)
                      return parsedReqE
        res <- liftIO $ runReaderT (runExceptT $ handler parsedReq) handlerState
        return (res, Just parsedReq)

    t2 <- liftIO getCurrentTime -- for measuring response time purposes

    -- apply the error modifier
    let modResult = fmapL qErrModifier result

    -- log and return result
    case modResult of
      Left err  -> let jErr = maybe (Left reqBody) (Right . A.toJSON) q
                  in logErrorAndResp (Just userInfo) requestId req jErr (isAdmin curRole) headers err
      Right res -> logSuccessAndResp (Just userInfo) requestId req res (Just (t1, t2)) headers

    where
      logger = scLogger serverCtx

      logErrorAndResp
        :: (MonadIO m)
        => Maybe UserInfo
        -> RequestId
        -> Wai.Request
        -> Either BL.ByteString A.Value
        -> Bool
        -> [HTTP.Header]
        -> QErr
        -> Spock.ActionCtxT ctx m a
      logErrorAndResp userInfo reqId req reqBody includeInternal headers qErr = do
        logError logger httpLogger userInfo reqId req reqBody qErr headers
        Spock.setStatus $ qeStatus qErr
        Spock.json $ qErrEncoder includeInternal qErr

      logSuccessAndResp userInfo reqId req result qTime headers = do
        logSuccess logger httpLogger userInfo reqId req (apiRespToLBS result) qTime headers
        case result of
          JSONResp (HttpResponse j h) -> do
            uncurry Spock.setHeader jsonHeader
            uncurry Spock.setHeader (requestIdHeader, unRequestId reqId)
            mapM_ (mapM_ (uncurry Spock.setHeader . unHeader)) h
            Spock.lazyBytes $ encJToLBS j
          RawResp (HttpResponse b h) -> do
            uncurry Spock.setHeader (requestIdHeader, unRequestId reqId)
            mapM_ (mapM_ (uncurry Spock.setHeader . unHeader)) h
            Spock.lazyBytes b

-- | a separate function to create the initialization context because some of
-- these contexts might be used by external functions
initialiseCtx
  :: (ToEngineLog a, MonadIO m)
  => HGECommand
  -> RawConnInfo
  -> Maybe LogCallbackFunction
  -> (HttpLogger a)
  -> (Maybe HttpResponseLogger)
  -> m (InitCtx a)
initialiseCtx hgeCmd rci logCallback httpLogger respLogger = do
  -- global http manager
  httpManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  instanceId <- liftIO generateInstanceId
  connInfo <- liftIO procConnInfo
  (loggers, pool) <- case hgeCmd of
    HCServe ServeOptions{..} -> do
      l@(Loggers _ logger pgLogger _ _) <- mkLoggers soEnabledLogTypes soLogLevel
      let sqlGenCtx = SQLGenCtx soStringifyNum
      -- log postgres connection info
      liftIO $ unLogger logger $ connInfoToLog connInfo
      pool <- liftIO $ Q.initPGPool connInfo soConnParams pgLogger

      -- safe init catalog
      initialise pool sqlGenCtx logger

      return (l, pool)

    _ -> do
      l@(Loggers _ _ pgLogger _ _) <- mkLoggers defaultEnabledLogTypes LevelInfo
      pool <- getMinimalPool pgLogger connInfo
      return (l, pool)

  -- get the unique db id
  eDbId <- liftIO $ runExceptT $ Q.runTx pool (Q.Serializable, Nothing) getDbId
  dbId <- either (liftIO . printErrJExit) return eDbId

  return $ InitCtx httpManager instanceId dbId loggers connInfo pool
  where
    initialise pool sqlGenCtx (Logger logger) = do
      currentTime <- liftIO getCurrentTime
      -- initialise the catalog
      initRes <- runAsAdmin pool sqlGenCtx $ initCatalogSafe currentTime
      either (liftIO . printErrJExit) (liftIO . logger . mkGenericStrLog LevelInfo "db_init") initRes

      -- migrate catalog if necessary
      migRes <- runAsAdmin pool sqlGenCtx $ migrateCatalog currentTime
      either (liftIO . printErrJExit) (liftIO . logger . mkGenericStrLog LevelInfo "db_migrate") migRes

    procConnInfo =
      either (printErrExit . connInfoErrModifier) return $ mkConnInfo rci

    getMinimalPool pgLogger ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      liftIO $ Q.initPGPool ci connParams pgLogger

    mkLoggers enabledLogs logLevel = do
      loggerCtx <- liftIO $ mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs logCallback
      let logger = mkLogger loggerCtx
          pgLogger = mkPGLogger logger
      return $ Loggers loggerCtx logger pgLogger httpLogger respLogger


runHGEServer
  :: (MonadIO m, ToEngineLog a, HasuraSpockAction m)
  => ServeOptions
  -> InitCtx a
  -> Maybe UserAuthMiddleware
  -> Maybe (HasuraMiddleware RQLQuery)
  -> Maybe ConsoleRenderer
  -> (forall b. m b -> IO b)
  -> m ()
runHGEServer so@(ServeOptions port host _ isoL mAdminSecret mAuthHook mJwtSecret mUnAuthRole corsCfg enableConsole consoleAssetsDir enableTelemetry strfyNum enabledAPIs lqOpts enableAL _ _) (InitCtx httpManager instanceId dbId loggers connInfo pgPool) authMiddleware metadataMiddleware renderConsole liftFn = do
  let sqlGenCtx = SQLGenCtx strfyNum

  let Loggers loggerCtx logger _ httpLogger respLogger = loggers

  initTime <- liftIO Clock.getCurrentTime
  -- log serve options
  liftIO $ unLogger logger $ serveOptsToLog so
  --hloggerCtx  <- mkLoggerCtx (defaultLoggerSettings False serverLogLevel) enabledLogs logCallback

  authModeRes <- liftIO $ runExceptT $ mkAuthMode mAdminSecret mAuthHook mJwtSecret
                                          mUnAuthRole httpManager loggerCtx

  authMode <- either (liftIO . printErrExit . T.unpack) return authModeRes


  (app, cacheRef, cacheInitTime) <-
    mkWaiApp isoL loggerCtx httpLogger respLogger sqlGenCtx enableAL pgPool connInfo httpManager
      authMode corsCfg enableConsole consoleAssetsDir enableTelemetry
      instanceId enabledAPIs lqOpts authMiddleware metadataMiddleware renderConsole liftFn

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> liftIO (getSCFromRef cacheRef)
  logInconsObjs logger inconsObjs

  -- start a background thread for schema sync
  startSchemaSync sqlGenCtx pgPool logger httpManager
                  cacheRef instanceId cacheInitTime

  let warpSettings = Warp.setPort port
                     . Warp.setHost host
                     . Warp.setGracefulShutdownTimeout (Just 30) -- 30s graceful shutdown
                     . Warp.setInstallShutdownHandler (shutdownHandler logger)
                     $ Warp.defaultSettings

  maxEvThrds <- liftIO $ getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
  evFetchMilliSec  <- liftIO $ getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
  logEnvHeaders <- liftIO $ getFromEnv False "LOG_HEADERS_FROM_ENV"

  -- prepare event triggers data
  prepareEvents pgPool logger
  eventEngineCtx <- liftIO $ atomically $ initEventEngineCtx maxEvThrds evFetchMilliSec
  let scRef = _scrCache cacheRef
  liftIO $ unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"
  void $ liftIO $ C.forkIO $ processEventQueue loggerCtx logEnvHeaders
    httpManager pgPool scRef eventEngineCtx

  -- start a background thread to check for updates
  void $ liftIO $ C.forkIO $ checkForUpdates loggerCtx httpManager

  -- start a background thread for telemetry
  when enableTelemetry $ do
    liftIO $ unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice
    void $ liftIO $ C.forkIO $ runTelemetry logger httpManager scRef dbId instanceId

  finishTime <- liftIO Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  liftIO $ unLogger logger $ mkGenericLog LevelInfo "server" $
    StartupTimeInfo "starting API server" apiInitTime
  liftIO $ Warp.runSettings warpSettings app

  where
    prepareEvents pool (Logger logger) = do
      liftIO $ logger $ mkGenericStrLog LevelInfo "event_triggers" "preparing data"
      res <- runTx pool unlockAllEvents
      either (liftIO . printErrJExit) return res

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
    shutdownHandler :: Logger -> IO () -> IO ()
    shutdownHandler  (Logger logger) closeSocket =
      void $ Signals.installHandler Signals.sigTERM (Signals.CatchOnce $ closeSocket >> logShutdown) Nothing
     where
      logShutdown = logger $
        mkGenericStrLog LevelInfo "server" "gracefully shutting down server"

runAsAdmin :: (MonadIO m) => Q.PGPool -> SQLGenCtx -> Run a -> m (Either QErr a)
runAsAdmin pool sqlGenCtx m = do
  httpManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  res  <- runExceptT $ peelRun emptySchemaCache adminUserInfo
          httpManager sqlGenCtx (PGExecCtx pool Q.Serializable) m
  return $ fmap fst res

telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
  <> "usage stats which allows us to keep improving Hasura at warp speed. "
  <> "To read more or opt-out, visit https://docs.hasura.io/1.0/graphql/manual/guides/telemetry.html"
