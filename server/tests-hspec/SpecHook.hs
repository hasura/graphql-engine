module SpecHook
  ( hook,
    setupTestEnvironment,
    teardownTestEnvironment,
  )
where

import Control.Exception.Safe (bracket)
import Harness.GraphqlEngine (startServerThread)
import Harness.TestEnvironment (TestEnvironment (..), stopServer)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, aroundAllWith)
import Text.Read (readMaybe)
import Prelude

setupTestEnvironment :: IO TestEnvironment
setupTestEnvironment = do
  murlPrefix <- lookupEnv "HASURA_TEST_URLPREFIX"
  mport <- fmap (>>= readMaybe) (lookupEnv "HASURA_TEST_PORT")
  server <- startServerThread ((,) <$> murlPrefix <*> mport)
  pure $ TestEnvironment server

teardownTestEnvironment :: TestEnvironment -> IO ()
teardownTestEnvironment TestEnvironment {server} =
  stopServer server

hook :: SpecWith TestEnvironment -> Spec
hook = aroundAllWith (const . bracket setupTestEnvironment teardownTestEnvironment)
