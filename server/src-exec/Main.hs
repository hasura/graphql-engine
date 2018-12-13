module Main where

import           Ops

import           Control.Monad.STM          (atomically)
import           Data.Time.Clock            (getCurrentTime)
import           Options.Applicative
import           System.Environment         (getEnvironment, lookupEnv)
import           System.Exit                (exitFailure, exitSuccess)

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
import           Hasura.RQL.Types           (QErr, RoleName (..), adminUserInfo,
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

data RavenOptions
  = RavenOptions
  { roConnInfo :: !RawConnInfo
  , roMode     :: !RavenMode
  } deriving (Show, Eq)

data ServeOptions
  = ServeOptions
  { soPort          :: !Int
  , soConnParams    :: !Q.ConnParams
  , soTxIso         :: !Q.TxIsolation
  , soRootDir       :: !(Maybe String)
  , soAccessKey     :: !(Maybe AccessKey)
  , soAuthHook      :: !(Maybe AuthHook)
  , soJwtSecret     :: !(Maybe Text)
  , soUnAuthRole    :: !(Maybe RoleName)
  , soCorsConfig    :: !CorsConfig
  , soEnableConsole :: !Bool
  } deriving (Show, Eq)

data RavenMode
  = ROServe !ServeOptions
  | ROExport
  | ROClean
  | ROExecute
  | ROVersion
  deriving (Show, Eq)

parseRavenMode :: Env -> Parser (Either String RavenMode)
parseRavenMode env =
  subparser
    ( command "serve" (info (helper <*> serveOptsParser)
          ( progDesc "Start the GraphQL Engine Server"
            <> footerDoc (Just serveCmdFooter)
          ))
        <> command "export" (info (pure $ Right ROExport)
          ( progDesc "Export graphql-engine's schema to stdout" ))
        <> command "clean" (info (pure $ Right ROClean)
          ( progDesc "Clean graphql-engine's metadata to start afresh" ))
        <> command "execute" (info (pure $ Right ROExecute)
          ( progDesc "Execute a query" ))
        <> command "version" (info (pure $ Right ROVersion)
          (progDesc "Prints the version of GraphQL Engine"))
    )
  where
    serveOptsParser = runConfig env serveOptsconfig
    serveOptsconfig = ROServe <$> serveOpts
    serveOpts = ServeOptions
                <$> configServerPort
                <*> configConnParams
                <*> configTxIsolation
                <*> configRootDir
                <*> configAccessKey
                <*> configWebHook
                <*> configJwtSecret
                <*> configUnAuthRole
                <*> configCorsConfig
                <*> configEnableConsole

parseArgs :: Env -> IO RavenOptions
parseArgs env = do
  eArgs <- execParser opts
  either ((>> exitFailure) . putStrLn) return eArgs
  where
    mkEitherRavenOpts a b = RavenOptions <$> a <*> b
    optParser = liftA2 mkEitherRavenOpts parseRawConnInfo $ parseRavenMode env
    parseRawConnInfo = runConfig env configRawConnInfo
    opts = info (helper <*> optParser)
           ( fullDesc <>
             header "Hasura GraphQL Engine: Expose Postgres over GraphQL APIs with access control" <>
             footerDoc (Just mainCmdFooter)
           )

printJSON :: (A.ToJSON a) => a -> IO ()
printJSON = BLC.putStrLn . A.encode

printYaml :: (A.ToJSON a) => a -> IO ()
printYaml = BC.putStrLn . Y.encode

printVersion :: RavenMode -> IO ()
printVersion = \case
  ROVersion -> putStrLn versionLine >> exitSuccess
  _         -> return ()
  where
    versionLine = "Hasura GraphQL Engine: " ++ T.unpack currentVersion

main :: IO ()
main =  do
  env <- getEnvironment
  (RavenOptions rci ravenMode) <- parseArgs env
  printVersion ravenMode
  ci <- either ((>> exitFailure) . putStrLn . connInfoErrModifier)
    return $ mkConnInfo rci
  printConnInfo ci
  loggerCtx   <- mkLoggerCtx $ defaultLoggerSettings True
  hloggerCtx  <- mkLoggerCtx $ defaultLoggerSettings False
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  case ravenMode of
    ROServe (ServeOptions port cp isoL mRootDir mAccessKey mAuthHook mJwtSecret
             mUnAuthRole corsCfg enableConsole) -> do

      authModeRes <- runExceptT $ mkAuthMode mAccessKey mAuthHook mJwtSecret
                                             mUnAuthRole httpManager loggerCtx

      am <- either ((>> exitFailure) . putStrLn . T.unpack) return authModeRes
      initialise ci httpManager
      -- migrate catalog if necessary
      migrate ci httpManager
      prepareEvents ci
      pool <- Q.initPGPool ci cp
      putStrLn $ "server: running on port " ++ show port
      (app, cacheRef) <- mkWaiApp isoL mRootDir loggerCtx pool httpManager
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

    ROExport -> do
      res <- runTx ci fetchMetadata
      either ((>> exitFailure) . printJSON) printJSON res
    ROClean -> do
      res <- runTx ci cleanCatalog
      either ((>> exitFailure) . printJSON) (const cleanSuccess) res
    ROExecute -> do
      queryBs <- BL.getContents
      res <- runAsAdmin ci httpManager $ execQuery queryBs
      either ((>> exitFailure) . printJSON) BLC.putStrLn res
    ROVersion -> return ()
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
      either ((>> exitFailure) . printJSON) putStrLn res
    migrate ci httpMgr = do
      currentTime <- getCurrentTime
      res <- runAsAdmin ci httpMgr $ migrateCatalog currentTime
      either ((>> exitFailure) . printJSON) putStrLn res
    prepareEvents ci = do
      putStrLn "event_triggers: preparing data"
      res <- runTx ci unlockAllEvents
      either ((>> exitFailure) . printJSON) return res

    getFromEnv :: (Read a) => a -> String -> IO a
    getFromEnv defaults env = do
      mEnv <- lookupEnv env
      let mRes = case mEnv of
            Nothing  -> Just defaults
            Just val -> readMaybe val
          eRes = maybe (Left $ "Wrong expected type for environment variable: " <> env) Right mRes
      either ((>> exitFailure) . putStrLn) return eRes

    cleanSuccess = putStrLn "successfully cleaned graphql-engine related data"

    printConnInfo ci =
      putStrLn $
        "Postgres connection info:"
        ++ "\n    Host: " ++ Q.connHost ci
        ++ "\n    Port: " ++ show (Q.connPort ci)
        ++ "\n    User: " ++ Q.connUser ci
        ++ "\n    Database: " ++ Q.connDatabase ci
