module SpecHook
  ( hook,
    setupTestEnvironment,
    teardownTestEnvironment,
  )
where

import Control.Exception.Safe (bracket)
import Data.UUID.V4 (nextRandom)
import Harness.GraphqlEngine (startServerThread)
import Harness.Logging
import Harness.TestEnvironment
  ( BackendSettings (..),
    TestEnvironment (..),
    stopServer,
  )
import Hasura.Prelude
import System.Environment (lookupEnv)
import System.Log.FastLogger qualified as FL
import Test.Hspec (Spec, SpecWith, aroundAllWith, runIO)
import Test.Hspec.Core.Spec (modifyConfig)

setupBackendSettings :: IO BackendSettings
setupBackendSettings = do
  postgresPort <-
    fmap
      (>>= readMaybe)
      (lookupEnv "HASURA_TEST_POSTGRES_PORT")
  pure
    ( BackendSettings
        { postgresSourcePort = postgresPort
        }
    )

setupTestEnvironment ::
  (Logger, IO ()) ->
  IO TestEnvironment
setupTestEnvironment (logger, loggerCleanup) = do
  murlPrefix <- lookupEnv "HASURA_TEST_URLPREFIX"
  mport <- fmap (>>= readMaybe) (lookupEnv "HASURA_TEST_PORT")
  backendSettings <- setupBackendSettings
  server <- startServerThread backendSettings ((,) <$> murlPrefix <*> mport)
  uniqueTestId <- nextRandom
  pure
    TestEnvironment
      { server = server,
        uniqueTestId = uniqueTestId,
        backendType = Nothing,
        logger = logger,
        loggerCleanup = loggerCleanup,
        backendSettings = backendSettings
      }

teardownTestEnvironment :: TestEnvironment -> IO ()
teardownTestEnvironment TestEnvironment {..} = do
  stopServer server

hook :: SpecWith TestEnvironment -> Spec
hook specs = do
  let logType = FL.LogFileNoRotate "tests-hspec.log" 1024
  (logger', cleanup) <- runIO $ FL.newFastLogger logType
  let logger = flLogger logger'

  modifyConfig (addLoggingFormatter logger)
  aroundAllWith
    (const . bracket (setupTestEnvironment (logger, cleanup)) teardownTestEnvironment)
    (contextualizeLogger specs)
