{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Monad.Trans.Except
import           Data.Time.Clock            (getCurrentTime)
import           Network.Wai                (Application)
import           Options.Applicative
import           System.Environment         (withArgs)
import           System.Exit                (exitFailure)
import           Test.Hspec.Core.Runner
import           Test.Hspec.Wai

import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Database.PG.Query          as Q
import qualified Hasura.Logging             as L
import           Hasura.Prelude
import           Hasura.Server.App          (mkWaiApp)
import           Hasura.Server.Auth         (AuthMode (..))


import qualified Database.PG.Query          as PGQ

import           Hasura.Server.Init
import           Ops                        (initCatalogSafe)
import           Spec                       (mkSpecs)

data ConnectionParams = ConnectionParams RawConnInfo Q.ConnParams

defTxMode :: Q.TxMode
defTxMode = (Q.Serializable, Nothing)

resetStateTx :: Q.TxE PGQ.PGExecErr ()
resetStateTx = do
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA hdb_catalog CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA hdb_views CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA public CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "CREATE SCHEMA public" () False

ravenApp :: L.LoggerCtx -> PGQ.PGPool -> AuthMode -> IO Application
ravenApp loggerCtx pool authMode = do
  let corsCfg = CorsConfigG "*" False -- cors is enabled
  -- spockAsApp $ spockT id $ app Q.Serializable Nothing rlogger pool AMNoAuth corsCfg True -- no access key and no webhook
  mkWaiApp Q.Serializable Nothing loggerCtx pool authMode corsCfg True

runTests :: PGQ.PGPool -> IO Application -> IO ()
runTests pool app = do
  -- reset state in the database
  void $ liftIO $ runExceptT $ Q.runTx pool defTxMode resetStateTx
  -- intialize state for graphql-engine in the database
  liftIO $ initialise pool
  -- generate the test specs
  specs <- mkSpecs
  -- run the tests
  withArgs [] $ hspecWith defaultConfig $ with app specs

  where
    initialise :: Q.PGPool -> IO ()
    initialise pool' = do
      currentTime <- getCurrentTime
      res <- runExceptT $ Q.runTx pool' defTxMode $ initCatalogSafe currentTime
      either ((>> exitFailure) . (BLC.putStrLn . J.encode)) putStrLn res


parseArgs :: IO ConnectionParams
parseArgs = execParser opts
  where
    optParser = ConnectionParams <$> parseRawConnInfo <*> parseConnParams

    opts = info (helper <*> optParser)
           ( fullDesc <>
             header "graphql-engine-test")

main :: IO ()
main = do
  -- parse CLI flags for connection params
  ConnectionParams rci cp <- parseArgs
  -- form the postgres connection info
  ci <- either ((>> exitFailure) . (putStrLn . connInfoErrModifier))
    return $ mkConnInfo Nothing rci
  -- intialize the pool and the logger
  pool <- Q.initPGPool ci cp
  loggerCtx <- L.mkLoggerCtx L.defaultLoggerSettings

  let am = AMAccessKeyAndHook "abcd" "https://us-central1-hasura-io.cloudfunctions.net/graphql-engine-webhook-test"
      noHookApp = ravenApp loggerCtx pool AMNoAuth
      appWHook  = ravenApp loggerCtx pool am

  putStrLn "Running tests without webhook"
  runTests pool noHookApp
  putStrLn "Running tests with webhook"
  runTests pool appWHook
