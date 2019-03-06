module Main where

import           Migrate                    (migrateCatalog)
import           Ops

import           Control.Monad.STM          (atomically)
import           Data.Time.Clock            (getCurrentTime)
import           Options.Applicative
import           System.Environment         (getEnvironment, lookupEnv)
import           System.Exit                (exitFailure)


import qualified Control.Concurrent         as C
import qualified Control.Concurrent.STM     as STM
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
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.Types           (adminUserInfo, emptySchemaCache)
import           Hasura.Server.App          (SchemaCacheRef (..), mkWaiApp)
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
                <*> parseEnableTelemetry
                <*> parseWsReadCookie
                <*> parseStringifyNum
                <*> parseEnabledAPIs


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
mkPGLogger (Logger logger) msg =
  logger $ PGLog LevelWarn msg

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  -- global http manager
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  loggerCtx   <- mkLoggerCtx $ defaultLoggerSettings True
  instanceId <- mkInstanceId
  let logger = mkLogger loggerCtx
      pgLogger = mkPGLogger logger
  case hgeCmd of
    HCServe so@(ServeOptions port host cp isoL mAdminSecret mAuthHook mJwtSecret
                mUnAuthRole corsCfg enableConsole enableTelemetry strfyNum enabledAPIs) -> do
      -- log serve options
      unLogger logger $ serveOptsToLog so
      hloggerCtx  <- mkLoggerCtx $ defaultLoggerSettings False

      authModeRes <- runExceptT $ mkAuthMode mAdminSecret mAuthHook mJwtSecret
                                             mUnAuthRole httpManager loggerCtx

      am <- either (printErrExit . T.unpack) return authModeRes

      ci <- procConnInfo rci
      -- log postgres connection info
      unLogger logger $ connInfoToLog ci

      -- create empty cache update events queue
      eventsQueue <- STM.newTQueueIO

      pool <- Q.initPGPool ci cp pgLogger

      -- start postgres cache update events listener thread in background
      listenerTId <- C.forkIO $ schemaUpdateEventListener pool logger eventsQueue
      unLogger logger $ mkThreadLog listenerTId instanceId TTListener

      -- safe init catalog
      initRes <- initialise logger ci httpManager

      -- prepare event triggers data
      prepareEvents logger ci

      (app, cacheRef, cacheBuiltTime) <-
        mkWaiApp isoL loggerCtx strfyNum pool httpManager am
          corsCfg enableConsole enableTelemetry instanceId enabledAPIs

      let scRef = _scrCache cacheRef

      -- start cache update events processor thread in background
      procTId <- C.forkIO $ schemaUpdateEventProcessor strfyNum pool logger httpManager
                              eventsQueue cacheRef instanceId cacheBuiltTime
      unLogger logger $ mkThreadLog procTId instanceId TTProcessor

      let warpSettings = Warp.setPort port $ Warp.setHost host Warp.defaultSettings

      maxEvThrds <- getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
      evFetchMilliSec  <- getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
      logEnvHeaders <- getFromEnv False "LOG_HEADERS_FROM_ENV"

      eventEngineCtx <- atomically $ initEventEngineCtx maxEvThrds evFetchMilliSec

      unLogger logger $
        mkGenericStrLog "event_triggers" "starting workers"
      void $ C.forkIO $ processEventQueue hloggerCtx logEnvHeaders httpManager pool scRef eventEngineCtx

      -- start a background thread to check for updates
      void $ C.forkIO $ checkForUpdates loggerCtx httpManager

      -- start a background thread for telemetry
      when enableTelemetry $ do
        unLogger logger $ mkGenericStrLog "telemetry" telemetryNotice
        void $ C.forkIO $ runTelemetry logger httpManager scRef initRes

      unLogger logger $
        mkGenericStrLog "server" "starting API server"
      Warp.runSettings warpSettings app

    HCExport -> do
      ci <- procConnInfo rci
      res <- runTx pgLogger ci fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      ci <- procConnInfo rci
      res <- runTx pgLogger ci cleanCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      queryBs <- BL.getContents
      ci <- procConnInfo rci
      res <- runAsAdmin pgLogger ci httpManager $ execQuery queryBs
      either printErrJExit BLC.putStrLn res

    HCVersion -> putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion
  where

    runTx pgLogger ci tx = do
      pool <- getMinimalPool pgLogger ci
      runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx

    runAsAdmin pgLogger ci httpManager m = do
      pool <- getMinimalPool pgLogger ci
      res  <- runExceptT $ peelRun emptySchemaCache adminUserInfo
              httpManager False pool Q.Serializable m
      return $ fmap fst res

    procConnInfo rci =
      either (printErrExit . connInfoErrModifier) return $
        mkConnInfo rci

    getMinimalPool pgLogger ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      Q.initPGPool ci connParams pgLogger

    initialise (Logger logger) ci httpMgr = do
      currentTime <- getCurrentTime
      let pgLogger = mkPGLogger $ Logger logger
      -- initialise the catalog
      initRes <- runAsAdmin pgLogger ci httpMgr $ initCatalogSafe currentTime
      either printErrJExit (logger . mkGenericStrLog "db_init") initRes

      -- migrate catalog if necessary
      migRes <- runAsAdmin pgLogger ci httpMgr $ migrateCatalog currentTime
      either printErrJExit (logger . mkGenericStrLog "db_migrate") migRes

      -- generate and retrieve uuids
      getUniqIds pgLogger ci

    prepareEvents (Logger logger) ci = do
      let pgLogger = mkPGLogger $ Logger logger
      logger $ mkGenericStrLog "event_triggers" "preparing data"
      res <- runTx pgLogger ci unlockAllEvents
      either printErrJExit return res

    mkThreadLog threadId instanceId threadType =
      let msg = T.pack (show threadType) <> " thread started"
      in StartupLog LevelInfo "threads" $
           A.object [ "instance_id" A..= getInstanceId instanceId
                    , "thread_id" A..= show threadId
                    , "message" A..= msg
                    ]

    getUniqIds pgLogger ci = do
      eDbId <- runTx pgLogger ci getDbId
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
