module SpecHook
  ( hook,
    setupTestEnvironment,
    teardownTestEnvironment,
  )
where

import Control.Exception.Safe (bracket)
import Data.UUID.V4 (nextRandom)
import Harness.GraphqlEngine (startServerThread)
import Harness.TestEnvironment (BackendSettings (..), TestEnvironment (..), stopServer)
import Hasura.Prelude
import System.Environment (lookupEnv)
import System.Log.FastLogger qualified as FL
import Test.Hspec (Spec, SpecWith, aroundAllWith)

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

setupTestEnvironment :: IO TestEnvironment
setupTestEnvironment = do
  murlPrefix <- lookupEnv "HASURA_TEST_URLPREFIX"
  mport <- fmap (>>= readMaybe) (lookupEnv "HASURA_TEST_PORT")
  backendSettings <- setupBackendSettings
  server <- startServerThread backendSettings ((,) <$> murlPrefix <*> mport)
  let logType = FL.LogFileNoRotate "tests-hspec.log" 1024
  (logger, loggerCleanup) <- FL.newFastLogger logType
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
  loggerCleanup

hook :: SpecWith TestEnvironment -> Spec
hook = aroundAllWith (const . bracket setupTestEnvironment teardownTestEnvironment)
