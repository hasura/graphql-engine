{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text.Encoding         as TE
import qualified Data.Yaml                  as Y
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import qualified Network.Wai.Handler.Warp   as Warp

import           Hasura.Events.HTTP         (HTTPSessionMgr (..))
import           Hasura.Events.Lib
import           Hasura.Logging             (defaultLoggerSettings, mkLoggerCtx)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.Server.App          (mkWaiApp)
import           Hasura.Server.Auth         (AccessKey (..), AuthMode (..),
                                             Webhook (..))
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Init

import qualified Database.PG.Query          as Q
import qualified Network.Connection         as NC
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
  , soCorsConfig    :: !CorsConfigFlags
  , soWebHook       :: !(Maybe Webhook)
  , soJwtSecret     :: !(Maybe Text)
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
                <*> parseCorsConfig
                <*> parseWebHook
                <*> parseJwtSecret
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

mkAuthMode :: Maybe AccessKey -> Maybe Webhook -> Maybe T.Text -> Either String AuthMode
mkAuthMode mAccessKey mWebHook mJwtSecret =
  case (mAccessKey, mWebHook, mJwtSecret) of
    (Nothing,  Nothing,   Nothing)     -> return AMNoAuth
    (Just key, Nothing,   Nothing)     -> return $ AMAccessKey key
    (Just key, Just hook, Nothing)     -> return $ AMAccessKeyAndHook key hook
    (Just key, Nothing,   Just jwtConf) -> do
      -- the JWT Conf as JSON string; try to parse it
      config <- A.eitherDecodeStrict $ TE.encodeUtf8 jwtConf
      return $ AMAccessKeyAndJWT key config

    (Nothing, Just _, Nothing)     -> throwError $
      "Fatal Error : --auth-hook (HASURA_GRAPHQL_AUTH_HOOK)"
      ++ " requires --access-key (HASURA_GRAPHQL_ACCESS_KEY) to be set"
    (Nothing, Nothing, Just _)     -> throwError $
      "Fatal Error : --jwt-secret (HASURA_GRAPHQL_JWT_SECRET)"
      ++ " requires --access-key (HASURA_GRAPHQL_ACCESS_KEY) to be set"
    (Nothing, Just _, Just _)     -> throwError
      "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"
    (Just _, Just _, Just _)     -> throwError
      "Fatal Error: Both webhook and JWT mode cannot be enabled at the same time"

main :: IO ()
main =  do
  (RavenOptions rci ravenMode) <- parseArgs
  mEnvDbUrl <- lookupEnv "HASURA_GRAPHQL_DATABASE_URL"
  ci <- either ((>> exitFailure) . putStrLn . connInfoErrModifier)
    return $ mkConnInfo mEnvDbUrl rci
  printConnInfo ci
  loggerCtx <- mkLoggerCtx $ defaultLoggerSettings True
  hloggerCtx <- mkLoggerCtx $ defaultLoggerSettings False
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  case ravenMode of
    ROServe (ServeOptions port cp isoL mRootDir mAccessKey corsCfg mWebHook mJwtSecret enableConsole) -> do
      mFinalAccessKey <- considerEnv "HASURA_GRAPHQL_ACCESS_KEY" $ getAccessKey <$> mAccessKey
      mFinalWebHook   <- considerEnv "HASURA_GRAPHQL_AUTH_HOOK" $ getWebhook <$> mWebHook
      mFinalJwtSecret <- considerEnv "HASURA_GRAPHQL_JWT_SECRET" mJwtSecret
      am <- either ((>> exitFailure) . putStrLn) return $
        mkAuthMode (AccessKey <$> mFinalAccessKey) (Webhook <$> mFinalWebHook) mFinalJwtSecret
      finalCorsDomain <- fromMaybe "*" <$> considerEnv "HASURA_GRAPHQL_CORS_DOMAIN" (ccDomain corsCfg)
      let finalCorsCfg =
            CorsConfigG finalCorsDomain $ ccDisabled corsCfg
      initialise ci
      migrate ci
      prepareEvents ci
      pool <- Q.initPGPool ci cp
      putStrLn $ "server: running on port " ++ show port
      (app, cacheRef) <- mkWaiApp isoL mRootDir loggerCtx pool httpManager am finalCorsCfg enableConsole
      let warpSettings = Warp.setPort port Warp.defaultSettings
                         -- Warp.setHost "*" Warp.defaultSettings

      -- start a background thread to check for updates
      void $ C.forkIO $ checkForUpdates loggerCtx httpManager

      maxEvThrds <- getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
      evPollSec  <- getFromEnv defaultPollingIntervalSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"

      eventEngineCtx <- atomically $ initEventEngineCtx maxEvThrds evPollSec
      httpSession    <- WrqS.newSessionControl Nothing TLS.tlsManagerSettings
      httpInsecureSession <- WrqS.newSessionControl Nothing (TLS.mkManagerSettings tlsInsecure Nothing)

      void $ C.forkIO $ processEventQueue hloggerCtx (HTTPSessionMgr httpSession httpInsecureSession) pool cacheRef eventEngineCtx

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
          eRes = maybe (Left "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE is not an integer") Right mRes
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
    tlsInsecure = NC.TLSSettingsSimple True False False
