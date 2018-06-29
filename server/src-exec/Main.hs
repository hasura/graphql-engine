{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Ops

import           Data.Time.Clock            (getCurrentTime)
import           Options.Applicative
import           System.Exit                (exitFailure)
import           Web.Spock.Core             (runSpockNoBanner, spockT)

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import qualified Data.Yaml                  as Y

import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.Server.App          (AuthMode (..), app, ravenLogGen)
import           Hasura.Server.Init
import           Hasura.Server.Logging      (withStdoutLogger)

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
  , soCorsConfig    :: !CorsConfig
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
      ( progDesc "Export raven's schema to stdout" ))
    <> command "clean" (info (pure ROClean)
      ( progDesc "Clean raven's metadata to start afresh" ))
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
             header "raven - Hasura's datastore")

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
      "Fatal Error : --auth-hook requires --access-key to be set"
    (Just key, Just hook) -> return $ AMAccessKeyAndHook key hook

main :: IO ()
main = withStdoutLogger ravenLogGen $ \rlogger -> do
  (RavenOptions rci ravenMode) <- parseArgs
  ci <- either ((>> exitFailure) . (putStrLn . connInfoErrModifier))
    return $ mkConnInfo rci
  printConnInfo ci
  case ravenMode of
    ROServe (ServeOptions port cp isoL mRootDir mAccessKey corsCfg mWebHook enableConsole) -> do
      am <- either ((>> exitFailure) . putStrLn) return $
        mkAuthMode mAccessKey mWebHook
      initialise ci
      migrate ci
      pool <- Q.initPGPool ci cp
      runSpockNoBanner port $ do
        putStrLn $ "server: running on port " ++ show port
        spockT id $ app isoL mRootDir rlogger pool am corsCfg enableConsole
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

    cleanSuccess = putStrLn "successfully cleaned raven related data"

    printConnInfo ci =
      putStrLn $
        "Postgres connection info:"
        ++ "\n    Host: " ++ Q.connHost ci
        ++ "\n    Port: " ++ show (Q.connPort ci)
        ++ "\n    User: " ++ Q.connUser ci
        ++ "\n    Database: " ++ Q.connDatabase ci
