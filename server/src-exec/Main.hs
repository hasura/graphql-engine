module Main where

import           Migrate                    (migrateCatalog)
import           Ops

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
import qualified Network.Wai.Handler.Warp   as Warp

import           Hasura.Db
import           Hasura.Events.Lib
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.Types           (SQLGenCtx (..), SchemaCache (..),
                                             adminUserInfo, emptySchemaCache)
import           Hasura.Server.App          (SchemaCacheRef (..), getSCFromRef,
                                             logInconsObjs, mkWaiApp)
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Query        (peelRun)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Utils
import           Hasura.Server.Version      (currentVersion)

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
                <*> parseReadOnlyDB


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

getIsolationLevel :: Bool -> Q.TxIsolation
getIsolationLevel readOnlyDb = if readOnlyDb then Q.RepeatableRead else Q.Serializable

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  -- global http manager
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  instanceId  <- mkInstanceId
  case hgeCmd of
    HCServe so@(ServeOptions port host cp isoL mAdminSecret mAuthHook
                mJwtSecret mUnAuthRole corsCfg enableConsole consoleAssetsDir
                enableTelemetry strfyNum enabledAPIs lqOpts enableAL
                enabledLogs serverLogLevel readOnlyDb) -> do

      let sqlGenCtx = SQLGenCtx strfyNum
      let isolationLevel = getIsolationLevel readOnlyDb

      (loggerCtx, logger, pgLogger) <- mkLoggers enabledLogs serverLogLevel

      initTime <- Clock.getCurrentTime
      -- log serve options
      unLogger logger $ serveOptsToLog so
      hloggerCtx  <- mkLoggerCtx (defaultLoggerSettings False serverLogLevel) enabledLogs

      authModeRes <- runExceptT $ mkAuthMode mAdminSecret mAuthHook mJwtSecret
                                             mUnAuthRole httpManager loggerCtx

      am <- either (printErrExit . T.unpack) return authModeRes

      ci <- procConnInfo rci
      -- log postgres connection info
      unLogger logger $ connInfoToLog ci

      pool <- Q.initPGPool ci cp pgLogger
      
      unLogger logger $ mkGenericStrLog LevelInfo "startup" "postgres connection established"

      -- safe init catalog
      initRes <- initialise pool sqlGenCtx readOnlyDb logger httpManager
      unLogger logger $ mkGenericStrLog LevelInfo "startup" "catalog initialised"

      (app, cacheRef, cacheInitTime) <-
        mkWaiApp isoL loggerCtx sqlGenCtx enableAL pool ci httpManager am
          corsCfg enableConsole consoleAssetsDir enableTelemetry
          instanceId enabledAPIs lqOpts

      -- log inconsistent schema objects
      inconsObjs <- scInconsistentObjs <$> getSCFromRef cacheRef
      logInconsObjs logger inconsObjs

      -- start a background thread for schema sync
      when (not readOnlyDb) $ startSchemaSync sqlGenCtx pool logger httpManager
                              cacheRef instanceId cacheInitTime
      --unLogger logger $ mkGenericStrLog LevelInfo "startup" "started schema sync"

      let warpSettings = Warp.setPort port $ Warp.setHost host Warp.defaultSettings

      maxEvThrds <- getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
      evFetchMilliSec  <- getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
      logEnvHeaders <- getFromEnv False "LOG_HEADERS_FROM_ENV"

      -- prepare event triggers data
      when (not readOnlyDb) (prepareEvents pool isolationLevel logger)
      eventEngineCtx <- atomically $ initEventEngineCtx maxEvThrds evFetchMilliSec
      let scRef = _scrCache cacheRef
      when (not readOnlyDb) (unLogger logger $
        mkGenericStrLog LevelInfo "event_triggers" "starting workers")
      when (not readOnlyDb) $ void $ C.forkIO $ processEventQueue hloggerCtx logEnvHeaders httpManager pool scRef eventEngineCtx

      -- start a background thread to check for updates
      void $ C.forkIO $ checkForUpdates loggerCtx httpManager

      -- start a background thread for telemetry
      when enableTelemetry $ do
        unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice
        void $ C.forkIO $ runTelemetry logger httpManager scRef initRes

      finishTime <- Clock.getCurrentTime
      let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
      unLogger logger $ mkGenericLog LevelInfo "server" $
        StartupTimeInfo "starting API server" apiInitTime
      Warp.runSettings warpSettings app

    HCExport -> do
      (_, _, pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      ci <- procConnInfo rci
      res <- runTx' pgLogger ci Q.Serializable fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      (_, _, pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      ci <- procConnInfo rci
      res <- runTx' pgLogger ci Q.Serializable cleanCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      (_, logger, pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      queryBs <- BL.getContents
      ci <- procConnInfo rci
      let sqlGenCtx = SQLGenCtx False
      pool <- getMinimalPool pgLogger ci
      res <- runAsAdmin pool sqlGenCtx Q.Serializable logger httpManager $ execQuery queryBs
      either printErrJExit BLC.putStrLn res

    HCVersion -> putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion
  where

    mkLoggers enabledLogs logLevel = do
      loggerCtx <- mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
      let logger = mkLogger loggerCtx
          pgLogger = mkPGLogger logger
      return (loggerCtx, logger, pgLogger)

    runTx pool isolationLevel tx =
      runExceptT $ Q.runTx pool (isolationLevel, Nothing) tx

    runTx' pgLogger ci isolationLevel tx = do
      pool <- getMinimalPool pgLogger ci
      runExceptT $ Q.runTx pool (isolationLevel, Nothing) tx

    runAsAdmin pool sqlGenCtx isolationLevel (Logger logger) httpManager m = do
      logger $ mkGenericStrLog LevelInfo "startup" "running runAsAdmin"
      res  <- runExceptT $ peelRun emptySchemaCache adminUserInfo
              httpManager sqlGenCtx (PGExecCtx pool isolationLevel) m
      return $ fmap fst res

    procConnInfo rci =
      either (printErrExit . connInfoErrModifier) return $
        mkConnInfo rci

    getMinimalPool pgLogger ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      Q.initPGPool ci connParams pgLogger

    initialise pool sqlGenCtx readOnlyDb (Logger logger) httpMgr = do
      currentTime <- getCurrentTime
      -- initialise the catalog
      initRes <- runAsAdmin pool sqlGenCtx (getIsolationLevel readOnlyDb) (Logger logger) httpMgr $
                 initCatalogSafe currentTime
      either printErrJExit (logger . mkGenericStrLog LevelInfo "db_init") initRes

      -- migrate catalog if necessary
      migRes <- runAsAdmin pool sqlGenCtx (getIsolationLevel readOnlyDb) (Logger logger) httpMgr $
                migrateCatalog currentTime
      when (not readOnlyDb) $ either printErrJExit (logger . mkGenericStrLog LevelInfo "db_migrate") migRes

      -- generate and retrieve uuids
      getUniqIds pool (getIsolationLevel readOnlyDb)

    prepareEvents pool isolationLevel (Logger logger) = do
      logger $ mkGenericStrLog LevelInfo "event_triggers" "preparing data"
      res <- runTx pool isolationLevel unlockAllEvents
      either printErrJExit return res

    getUniqIds pool isolationLevel = do
      eDbId <- runTx pool isolationLevel getDbId
      dbId <- either printErrJExit return eDbId
      fp <- liftIO generateFingerprint
      return (dbId, fp)

    getFromEnv :: (Read a) => a -> String -> IO a
    getFromEnv defaults env = do
      mEnv <- lookupEnv env
      let mRes = case mEnv of
            Nothing  -> Just defaults
            Just val -> readMaybe val
          eRes = maybe (Left $ "Wrong expected type for environment variable: " <> env) Right mRes
      either printErrExit return eRes

    cleanSuccess =
      putStrLn "successfully cleaned graphql-engine related data"


telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
  <> "usage stats which allows us to keep improving Hasura at warp speed. "
  <> "To read more or opt-out, visit https://docs.hasura.io/1.0/graphql/manual/guides/telemetry.html"
