{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Ops

import           Control.Monad.STM          (atomically)
import           Data.Time.Clock            (getCurrentTime)
import           Options.Applicative
import           System.Environment         (lookupEnv)
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
import           Hasura.RQL.Types           (RoleName (..))
import           Hasura.Server.App          (mkWaiApp)
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Init

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
  , soWebHook       :: !(Maybe Webhook)
  , soJwtSecret     :: !(Maybe Text)
  , soUnAuthRole    :: !(Maybe RoleName)
  , soCorsConfig    :: !CorsConfigFlags
  , soEnableConsole :: !Bool
  } deriving (Show, Eq)

data RavenMode
  = ROServe !ServeOptions
  | ROExport
  | ROClean
  | ROExecute
  deriving (Show, Eq)

parseRavenMode :: Parser RavenMode
parseRavenMode = subparser
  ( command "serve" (info (helper <*> serveOptsParser)
      ( progDesc "Start the HTTP api server" ))
    <> command "export" (info (pure ROExport)
      ( progDesc "Export graphql-engine's schema to stdout" ))
    <> command "clean" (info (pure ROClean)
      ( progDesc "Clean graphql-engine's metadata to start afresh" ))
    <> command "execute" (info (pure ROExecute)
      ( progDesc "Execute a query" ))
  )
  where
    serveOptsParser = ROServe <$> serveOpts
    serveOpts = ServeOptions
                <$> parseServerPort
                <*> parseConnParams
                <*> parseTxIsolation
                <*> parseRootDir
                <*> parseAccessKey
                <*> parseWebHook
                <*> parseJwtSecret
                <*> parseUnAuthRole
                <*> parseCorsConfig
                <*> parseEnableConsole

parseArgs :: IO RavenOptions
parseArgs = execParser opts
  where
    optParser = RavenOptions <$> parseRawConnInfo <*> parseRavenMode
    opts = info (helper <*> optParser)
           ( fullDesc <>
             header "Hasura's graphql-engine - Exposes Postgres over GraphQL")

printJSON :: (A.ToJSON a) => a -> IO ()
printJSON = BLC.putStrLn . A.encode

printYaml :: (A.ToJSON a) => a -> IO ()
printYaml = BC.putStrLn . Y.encode

getEnableConsoleEnv :: IO Bool
getEnableConsoleEnv = do
  mVal <- fmap T.pack <$> lookupEnv enableConsoleEnvVar
  maybe (return False) (parseAsBool . T.toLower) mVal
  where
    enableConsoleEnvVar = "HASURA_GRAPHQL_ENABLE_CONSOLE"
    truthVals = ["true", "t", "yes", "y"]
    falseVals = ["false", "f", "no", "n"]

    parseAsBool t
      | t `elem` truthVals = return True
      | t `elem` falseVals = return False
      | otherwise = putStrLn errMsg >> exitFailure

    errMsg = "Fatal Error: " ++ enableConsoleEnvVar
             ++ " is not valid boolean text. " ++ "True values are "
             ++ show truthVals ++ " and  False values are " ++ show falseVals
             ++ ". All values are case insensitive"

main :: IO ()
main =  do
  (RavenOptions rci ravenMode) <- parseArgs
  mEnvDbUrl <- lookupEnv "HASURA_GRAPHQL_DATABASE_URL"
  ci <- either ((>> exitFailure) . putStrLn . connInfoErrModifier)
    return $ mkConnInfo mEnvDbUrl rci
  printConnInfo ci
  loggerCtx   <- mkLoggerCtx $ defaultLoggerSettings True
  hloggerCtx  <- mkLoggerCtx $ defaultLoggerSettings False
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  case ravenMode of
    ROServe (ServeOptions port cp isoL mRootDir mAccessKey mWebHook mJwtSecret
             mUnAuthRole corsCfg enableConsole) -> do

      -- get all auth mode related config
      mFinalAccessKey <- considerEnv "HASURA_GRAPHQL_ACCESS_KEY" $ getAccessKey <$> mAccessKey
      mFinalWebHook   <- considerEnv "HASURA_GRAPHQL_AUTH_HOOK" $ getWebhook <$> mWebHook
      mFinalJwtSecret <- considerEnv "HASURA_GRAPHQL_JWT_SECRET" mJwtSecret
      mFinalUnAuthRole <- considerEnv "HASURA_GRAPHQL_UNAUTHORIZED_ROLE" $ getRoleTxt <$> mUnAuthRole
      -- prepare auth mode
      authModeRes <- runExceptT $ mkAuthMode (AccessKey <$> mFinalAccessKey)
                                             (Webhook <$> mFinalWebHook)
                                             mFinalJwtSecret
                                             (RoleName <$> mFinalUnAuthRole)
                                             httpManager
                                             loggerCtx
      am <- either ((>> exitFailure) . putStrLn . T.unpack) return authModeRes
      finalCorsDomain <- fromMaybe "*" <$> considerEnv "HASURA_GRAPHQL_CORS_DOMAIN" (ccDomain corsCfg)
      let finalCorsCfg = CorsConfigG finalCorsDomain $ ccDisabled corsCfg
      -- enable console config
      finalEnableConsole <- bool getEnableConsoleEnv (return True) enableConsole
      -- init catalog if necessary
      initialise ci
      -- migrate catalog if necessary
      migrate ci
      prepareEvents ci
      pool <- Q.initPGPool ci cp
      putStrLn $ "server: running on port " ++ show port
      (app, cacheRef) <- mkWaiApp isoL mRootDir loggerCtx pool httpManager
                         am finalCorsCfg finalEnableConsole
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
      res <- runTx ci $ execQuery queryBs
      either ((>> exitFailure) . printJSON) BLC.putStrLn res
  where
    runTx ci tx = do
      pool <- getMinimalPool ci
      runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx
    getMinimalPool ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      Q.initPGPool ci connParams
    initialise ci = do
      currentTime <- getCurrentTime
      res <- runTx ci $ initCatalogSafe currentTime
      either ((>> exitFailure) . printJSON) putStrLn res
    migrate ci = do
      currentTime <- getCurrentTime
      res <- runTx ci $ migrateCatalog currentTime
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

    -- if flags given are Nothing consider it's value from Env
    considerEnv _ (Just t) = return $ Just t
    considerEnv e Nothing  = fmap T.pack <$> lookupEnv e
