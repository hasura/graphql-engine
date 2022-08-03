module SpecHook
  ( hook,
    setupTestEnvironment,
    teardownTestEnvironment,
  )
where

import Control.Exception.Safe (bracket)
import Harness.GraphqlEngine (startServerThread)
import Harness.TestEnvironment (TestEnvironment (..), stopServer)
import Hasura.Prelude
import System.Environment (lookupEnv)
import System.Log.FastLogger qualified as FL
import Test.Hspec (Spec, SpecWith, aroundAllWith)

setupTestEnvironment :: IO TestEnvironment
setupTestEnvironment = do
  murlPrefix <- lookupEnv "HASURA_TEST_URLPREFIX"
  mport <- fmap (>>= readMaybe) (lookupEnv "HASURA_TEST_PORT")
  server <- startServerThread ((,) <$> murlPrefix <*> mport)
  let logType = FL.LogFileNoRotate "tests-hspec.log" 1024
  (logger, loggerCleanup) <- FL.newFastLogger logType
  pure TestEnvironment {..}

teardownTestEnvironment :: TestEnvironment -> IO ()
teardownTestEnvironment TestEnvironment {..} = do
  stopServer server
  loggerCleanup

hook :: SpecWith TestEnvironment -> Spec
hook = aroundAllWith (const . bracket setupTestEnvironment teardownTestEnvironment)
