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

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  -- global http manager
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  instanceId  <- generateInstanceId
  case hgeCmd of
    HCServe so@(ServeOptions port host cp isoL mAdminSecret mAuthHook
                mJwtSecret mUnAuthRole corsCfg enableConsole consoleAssetsDir
                enableTelemetry strfyNum enabledAPIs lqOpts enableAL
                enabledLogs serverLogLevel) -> do

      let sqlGenCtx = SQLGenCtx strfyNum

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

      -- safe init catalog
      dbId <- initialise pool sqlGenCtx logger httpManager

      (app, cacheRef, cacheInitTime) <-
        mkWaiApp isoL loggerCtx sqlGenCtx enableAL pool ci httpManager am
          corsCfg enableConsole consoleAssetsDir enableTelemetry
          instanceId enabledAPIs lqOpts

      -- log inconsistent schema objects
      inconsObjs <- scInconsistentObjs <$> getSCFromRef cacheRef
      logInconsObjs logger inconsObjs

      -- start a background thread for schema sync
      startSchemaSync sqlGenCtx pool logger httpManager
                      cacheRef instanceId cacheInitTime

      let warpSettings = Warp.setPort port $ Warp.setHost host Warp.defaultSettings

      maxEvThrds <- getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
      evFetchMilliSec  <- getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
      logEnvHeaders <- getFromEnv False "LOG_HEADERS_FROM_ENV"

      -- prepare event triggers data
      prepareEvents pool logger
      eventEngineCtx <- atomically $ initEventEngineCtx maxEvThrds evFetchMilliSec
      let scRef = _scrCache cacheRef
      unLogger logger $
        mkGenericStrLog LevelInfo "event_triggers" "starting workers"
      void $ C.forkIO $ processEventQueue hloggerCtx logEnvHeaders
        httpManager pool scRef eventEngineCtx

      -- start a background thread to check for updates
      void $ C.forkIO $ checkForUpdates loggerCtx httpManager

      -- start a background thread for telemetry
      when enableTelemetry $ do
        unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice
        void $ C.forkIO $ runTelemetry logger httpManager scRef dbId instanceId

      finishTime <- Clock.getCurrentTime
      let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
      unLogger logger $ mkGenericLog LevelInfo "server" $
        StartupTimeInfo "starting API server" apiInitTime
      Warp.runSettings warpSettings app

    HCExport -> do
      (_, _, pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      ci <- procConnInfo rci
      res <- runTx' pgLogger ci fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      (_, _, pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      ci <- procConnInfo rci
      res <- runTx' pgLogger ci cleanCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      (_, _, pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      queryBs <- BL.getContents
      ci <- procConnInfo rci
      let sqlGenCtx = SQLGenCtx False
      pool <- getMinimalPool pgLogger ci
      res <- runAsAdmin pool sqlGenCtx httpManager $ execQuery queryBs
      either printErrJExit BLC.putStrLn res

    HCVersion -> putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion
  where

    mkLoggers enabledLogs logLevel = do
      loggerCtx <- mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
      let logger = mkLogger loggerCtx
          pgLogger = mkPGLogger logger
      return (loggerCtx, logger, pgLogger)

    runTx pool tx =
      runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx

    runTx' pgLogger ci tx = do
      pool <- getMinimalPool pgLogger ci
      runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx

    runAsAdmin pool sqlGenCtx httpManager m = do
      res  <- runExceptT $ peelRun emptySchemaCache adminUserInfo
              httpManager sqlGenCtx (PGExecCtx pool Q.Serializable) m
      return $ fmap fst res

    procConnInfo rci =
      either (printErrExit . connInfoErrModifier) return $
        mkConnInfo rci

    getMinimalPool pgLogger ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      Q.initPGPool ci connParams pgLogger

    initialise pool sqlGenCtx (Logger logger) httpMgr = do
      currentTime <- getCurrentTime
      -- initialise the catalog
      initRes <- runAsAdmin pool sqlGenCtx httpMgr $
                 initCatalogSafe currentTime
      either printErrJExit (logger . mkGenericStrLog LevelInfo "db_init") initRes

      -- migrate catalog if necessary
      migRes <- runAsAdmin pool sqlGenCtx httpMgr $
                migrateCatalog currentTime
      either printErrJExit (logger . mkGenericStrLog LevelInfo "db_migrate") migRes

      -- retrieve database id
      eDbId <- runTx pool getDbId
      either printErrJExit return eDbId

    prepareEvents pool (Logger logger) = do
      logger $ mkGenericStrLog LevelInfo "event_triggers" "preparing data"
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

    cleanSuccess =
      putStrLn "successfully cleaned graphql-engine related data"


telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
  <> "usage stats which allows us to keep improving Hasura at warp speed. "
  <> "To read more or opt-out, visit https://docs.hasura.io/1.0/graphql/manual/guides/telemetry.html"
