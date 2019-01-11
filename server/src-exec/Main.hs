module Main where

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
import qualified Data.Yaml                  as Y
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import qualified Network.Wai.Handler.Warp   as Warp

import           Hasura.Events.Lib
import           Hasura.Logging             (Logger (..), defaultLoggerSettings,
                                             mkLogger, mkLoggerCtx)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.Types           (QErr, adminUserInfo,
                                             emptySchemaCache)
import           Hasura.Server.App          (mkWaiApp)
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Query        (peelRun)
import           Hasura.Server.Telemetry
import           Hasura.Server.Version      (currentVersion)

import qualified Database.PG.Query          as Q
import qualified Network.HTTP.Client.TLS    as TLS
import qualified Network.Wreq.Session       as WrqS

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
                <*> parseAccessKey
                <*> parseWebHook
                <*> parseJwtSecret
                <*> parseUnAuthRole
                <*> parseCorsConfig
                <*> parseEnableConsole
                <*> parseEnableTelemetry

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

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  -- global http manager
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  loggerCtx   <- mkLoggerCtx $ defaultLoggerSettings True
  let logger = mkLogger loggerCtx
  case hgeCmd of
    HCServe so@(ServeOptions port host cp isoL mAccessKey mAuthHook mJwtSecret
             mUnAuthRole corsCfg enableConsole enableTelemetry) -> do
      -- log serve options
      unLogger logger $ serveOptsToLog so
      hloggerCtx  <- mkLoggerCtx $ defaultLoggerSettings False

      authModeRes <- runExceptT $ mkAuthMode mAccessKey mAuthHook mJwtSecret
                                             mUnAuthRole httpManager loggerCtx

      am <- either (printErrExit . T.unpack) return authModeRes

      ci <- procConnInfo rci
      -- log postgres connection info
      unLogger logger $ connInfoToLog ci
      -- safe init catalog
      initialise logger ci httpManager
      -- migrate catalog if necessary
      migrate logger ci httpManager
      -- prepare event triggers data
      prepareEvents logger ci

      pool <- Q.initPGPool ci cp
      (app, cacheRef) <- mkWaiApp isoL loggerCtx pool httpManager
                         am corsCfg enableConsole enableTelemetry

      let warpSettings = Warp.setPort port $ Warp.setHost host Warp.defaultSettings

      maxEvThrds <- getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
      evFetchMilliSec  <- getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
      logEnvHeaders <- getFromEnv False "LOG_HEADERS_FROM_ENV"

      eventEngineCtx <- atomically $ initEventEngineCtx maxEvThrds evFetchMilliSec
      httpSession    <- WrqS.newSessionControl Nothing TLS.tlsManagerSettings

      unLogger logger $
        mkGenericStrLog "event_triggers" "starting workers"
      void $ C.forkIO $ processEventQueue hloggerCtx logEnvHeaders httpSession pool cacheRef eventEngineCtx

      -- start a background thread to check for updates
      void $ C.forkIO $ checkForUpdates loggerCtx httpManager

      -- start a background thread for telemetry
      when enableTelemetry $ do
        unLogger logger $ mkGenericStrLog "telemetry" telemetryNotice
        res <- getUniqIds ci
        runEither res (logTelemetryErr logger) $
          void . C.forkIO . runTelemetry logger httpManager cacheRef

      unLogger logger $
        mkGenericStrLog "server" "starting API server"
      Warp.runSettings warpSettings app

    HCExport -> do
      ci <- procConnInfo rci
      res <- runTx ci fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      ci <- procConnInfo rci
      res <- runTx ci cleanCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      queryBs <- BL.getContents
      ci <- procConnInfo rci
      res <- runAsAdmin ci httpManager $ execQuery queryBs
      either printErrJExit BLC.putStrLn res

    HCVersion -> putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion
  where

    runTx :: Q.ConnInfo -> Q.TxE QErr a -> IO (Either QErr a)
    runTx ci tx = do
      pool <- getMinimalPool ci
      runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx

    runAsAdmin ci httpManager m = do
      pool <- getMinimalPool ci
      res  <- runExceptT $ peelRun emptySchemaCache adminUserInfo
              httpManager pool Q.Serializable m
      return $ fmap fst res

    procConnInfo rci =
      either (printErrExit . connInfoErrModifier) return $
        mkConnInfo rci

    getMinimalPool ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      Q.initPGPool ci connParams

    initialise (Logger logger) ci httpMgr = do
      currentTime <- getCurrentTime
      res <- runAsAdmin ci httpMgr $ initCatalogSafe currentTime
      either printErrJExit (logger . mkGenericStrLog "db_init") res

    migrate (Logger logger) ci httpMgr = do
      currentTime <- getCurrentTime
      res <- runAsAdmin ci httpMgr $ migrateCatalog currentTime
      either printErrJExit (logger . mkGenericStrLog "db_migrate") res

    prepareEvents (Logger logger) ci = do
      logger $ mkGenericStrLog "event_triggers" "preparing data"
      res <- runTx ci unlockAllEvents
      either printErrJExit return res

    getUniqIds ci =
      runTx ci $ do
        dbId <- getDbId
        fp <- generateFingerprint
        return (dbId, fp)

    logTelemetryErr (Logger logger) err = do
      let err' = T.pack $ BLC.unpack $ A.encode err
      logger $ mkTelemetryLog "initialise_error"
               ("failed to start telemetry: " <> err') Nothing

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

    runEither ev lAction rAction = either lAction rAction ev


telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
  <> "usage stats which allows us to keep improving Hasura at warp speed. "
  <> "To read more or opt-out, visit https://docs.hasura.io/1.0/graphql/manual/guides/telemetry.html"
