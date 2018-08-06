{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Ops

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

import           Hasura.Logging             (defaultLoggerSettings, mkLoggerCtx)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.Server.App          (mkWaiApp)
import           Hasura.Server.Auth         (AuthMode (..))
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Init

import qualified Database.PG.Query          as Q

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
  , soWebHook       :: !(Maybe T.Text)
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

mkAuthMode :: Maybe AccessKey -> Maybe T.Text -> Either String AuthMode
mkAuthMode mAccessKey mWebHook =
  case (mAccessKey, mWebHook) of
    (Nothing, Nothing)    -> return AMNoAuth
    (Just key, Nothing)   -> return $ AMAccessKey key
    (Nothing, Just _)     -> throwError $
      "Fatal Error : --auth-hook (HASURA_GRAPHQL_AUTH_HOOK)"
      ++ " requires --access-key (HASURA_GRAPHQL_ACCESS_KEY) to be set"
    (Just key, Just hook) -> return $ AMAccessKeyAndHook key hook

main :: IO ()
main =  do
  (RavenOptions rci ravenMode) <- parseArgs
  mEnvDbUrl <- lookupEnv "HASURA_GRAPHQL_DATABASE_URL"
  ci <- either ((>> exitFailure) . putStrLn . connInfoErrModifier)
    return $ mkConnInfo mEnvDbUrl rci
  printConnInfo ci
  loggerCtx <- mkLoggerCtx defaultLoggerSettings
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  case ravenMode of
    ROServe (ServeOptions port cp isoL mRootDir mAccessKey corsCfg mWebHook enableConsole) -> do

      mFinalAccessKey <- considerEnv "HASURA_GRAPHQL_ACCESS_KEY" mAccessKey
      mFinalWebHook <- considerEnv "HASURA_GRAPHQL_AUTH_HOOK" mWebHook
      am <- either ((>> exitFailure) . putStrLn) return $
        mkAuthMode mFinalAccessKey mFinalWebHook
      finalCorsDomain <- fromMaybe "*" <$> considerEnv "HASURA_GRAPHQL_CORS_DOMAIN" (ccDomain corsCfg)
      let finalCorsCfg =
            CorsConfigG finalCorsDomain $ ccDisabled corsCfg
      initialise ci
      migrate ci
      pool <- Q.initPGPool ci cp
      putStrLn $ "server: running on port " ++ show port
      app <- mkWaiApp isoL mRootDir loggerCtx pool httpManager am finalCorsCfg enableConsole
      let warpSettings = Warp.setPort port Warp.defaultSettings
                         -- Warp.setHost "*" Warp.defaultSettings

      -- start a background thread to check for updates
      void $ C.forkIO $ checkForUpdates loggerCtx httpManager

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
