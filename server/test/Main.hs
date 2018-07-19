{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.Except
import           Data.Time.Clock            (getCurrentTime)
import           Network.Wai                (Application)
import           Options.Applicative
import           System.Environment         (withArgs)
import           System.Exit                (exitFailure)
import           Test.Hspec.Core.Runner
import           Test.Hspec.Wai
import           Web.Spock.Core             (spockAsApp, spockT)

import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Hasura.Prelude
import           Hasura.Server.App          (AuthMode (..), RavenLogger, app,
                                             ravenLogGen)
import           Hasura.Server.Init
import           Hasura.Server.Logging      (withStdoutLogger)
import           Ops                        (initCatalogSafe)
import           Spec                       (mkSpecs)

import qualified Database.PG.Query          as Q
import qualified Database.PG.Query          as PGQ


data ConnectionParams = ConnectionParams RawConnInfo Q.ConnParams

defTxMode :: Q.TxMode
defTxMode = (Q.Serializable, Nothing)

resetStateTx :: Q.TxE PGQ.PGExecErr ()
resetStateTx = do
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA hdb_catalog CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA hdb_views CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA public CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "CREATE SCHEMA public" () False

ravenApp :: RavenLogger -> PGQ.PGPool -> IO Application
ravenApp rlogger pool = do
  let corsCfg = CorsConfigG "*" False -- cors is disabled
  spockAsApp $ spockT id $ app Q.Serializable Nothing rlogger pool AMNoAuth corsCfg True -- no access key and no webhook

main :: IO ()
main = withStdoutLogger ravenLogGen $ \rlogger -> do
  -- parse CLI flags for connection params
  ConnectionParams rci cp <- parseArgs
  -- form the postgres connection info
  ci <- either ((>> exitFailure) . (putStrLn . connInfoErrModifier))
    return $ mkConnInfo Nothing rci
  -- intialize the pool
  pool <- Q.initPGPool ci cp
  -- reset state in the database
  void $ liftIO $ runExceptT $ Q.runTx pool defTxMode resetStateTx
  -- intialize state for graphql-engine in the database
  liftIO $ initialise pool
  -- generate the test specs
  specs <- mkSpecs
  -- run the tests
  withArgs [] $ hspecWith defaultConfig $ with (ravenApp rlogger pool) specs

  where
    initialise :: Q.PGPool -> IO ()
    initialise pool = do
      currentTime <- getCurrentTime
      res <- runExceptT $ Q.runTx pool defTxMode $ initCatalogSafe currentTime
      either ((>> exitFailure) . (BLC.putStrLn . J.encode)) putStrLn res


parseArgs :: IO ConnectionParams
parseArgs = execParser opts
  where
    optParser = ConnectionParams <$> parseRawConnInfo <*> parseConnParams

    opts = info (helper <*> optParser)
           ( fullDesc <>
             header "graphql-engine-test")
