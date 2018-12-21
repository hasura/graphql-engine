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
import           Hasura.Logging             (defaultLoggerSettings, mkLoggerCtx)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.Types           (QErr, adminUserInfo,
                                             emptySchemaCache)
import           Hasura.Server.App          (mkWaiApp)
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Query        (peelRun)
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
                <*> parseConnParams
                <*> parseTxIsolation
                <*> parseAccessKey
                <*> parseWebHook
                <*> parseJwtSecret
                <*> parseUnAuthRole
                <*> parseCorsConfig
                <*> parseEnableConsole

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

procConnInfo :: RawConnInfo -> IO Q.ConnInfo
procConnInfo rci = do
  ci <- either (printErrExit . connInfoErrModifier)
        return $ mkConnInfo rci
  printConnInfo ci
  return ci
  where
    printConnInfo ci =
      putStrLn $
        "Postgres connection info:"
        ++ "\n    Host: " ++ Q.connHost ci
        ++ "\n    Port: " ++ show (Q.connPort ci)
        ++ "\n    User: " ++ Q.connUser ci
        ++ "\n    Database: " ++ Q.connDatabase ci

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  -- global http manager
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  case hgeCmd of
    HCServe (ServeOptions port cp isoL mAccessKey mAuthHook mJwtSecret
             mUnAuthRole corsCfg enableConsole) -> do
      loggerCtx   <- mkLoggerCtx $ defaultLoggerSettings True
      hloggerCtx  <- mkLoggerCtx $ defaultLoggerSettings False

      authModeRes <- runExceptT $ mkAuthMode mAccessKey mAuthHook mJwtSecret
                                             mUnAuthRole httpManager loggerCtx

      am <- either (printErrExit . T.unpack) return authModeRes
      ci <- procConnInfo rci
      initialise ci httpManager
      -- migrate catalog if necessary
      migrate ci httpManager
      prepareEvents ci
      pool <- Q.initPGPool ci cp
      putStrLn $ "server: running on port " ++ show port
      (app, cacheRef) <- mkWaiApp isoL loggerCtx pool httpManager
                         am corsCfg enableConsole
      let warpSettings = Warp.setPort port Warp.defaultSettings
                         -- Warp.setHost "*" Warp.defaultSettings

      -- start a background thread to check for updates
      void $ C.forkIO $ checkForUpdates loggerCtx httpManager

      maxEvThrds <- getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
      evFetchMilliSec  <- getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
      logEnvHeaders <- getFromEnv False "LOG_HEADERS_FROM_ENV"

      eventEngineCtx <- atomically $ initEventEngineCtx maxEvThrds evFetchMilliSec
      httpSession    <- WrqS.newSessionControl Nothing TLS.tlsManagerSettings

      void $ C.forkIO $ processEventQueue hloggerCtx logEnvHeaders httpSession pool cacheRef eventEngineCtx

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
    getMinimalPool ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      Q.initPGPool ci connParams
    initialise ci httpMgr = do
      currentTime <- getCurrentTime
      res <- runAsAdmin ci httpMgr $ initCatalogSafe currentTime
      either printErrJExit putStrLn res
    migrate ci httpMgr = do
      currentTime <- getCurrentTime
      res <- runAsAdmin ci httpMgr $ migrateCatalog currentTime
      either printErrJExit putStrLn res
    prepareEvents ci = do
      putStrLn "event_triggers: preparing data"
      res <- runTx ci unlockAllEvents
      either printErrJExit return res

    getFromEnv :: (Read a) => a -> String -> IO a
    getFromEnv defaults env = do
      mEnv <- lookupEnv env
      let mRes = case mEnv of
            Nothing  -> Just defaults
            Just val -> readMaybe val
          eRes = maybe (Left $ "Wrong expected type for environment variable: " <> env) Right mRes
      either printErrExit return eRes

    cleanSuccess = putStrLn "successfully cleaned graphql-engine related data"
