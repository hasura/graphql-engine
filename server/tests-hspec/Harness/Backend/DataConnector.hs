{-# LANGUAGE QuasiQuotes #-}

-- | Data Connector helpers.
module Harness.Backend.DataConnector
  ( -- * Reference Agent
    setupFixture,
    teardown,
    defaultBackendConfig,

    -- * Mock Agent
    MockConfig (..),
    MockAgentEnvironment (..),
    TestCase (..),
    mockBackendConfig,
    chinookMock,
    runMockedTest,
    mkLocalTestEnvironmentMock,
    setupMock,
    teardownMock,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent (ThreadId, forkIO, killThread)
import Data.Aeson qualified as Aeson
import Data.IORef qualified as I
import Harness.Backend.DataConnector.MockAgent
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context (BackendType (DataConnector), Options, defaultBackendTypeString)
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (shouldBe)

--------------------------------------------------------------------------------

defaultBackendConfig :: Aeson.Value
defaultBackendConfig =
  let backendType = defaultBackendTypeString $ DataConnector
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65005/"
|]

mockBackendConfig :: Aeson.Value
mockBackendConfig =
  let backendType = defaultBackendTypeString $ DataConnector
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65006/"
|]

--------------------------------------------------------------------------------
-- Chinook Agent

-- | Setup the schema given source metadata and backend config.
setupFixture :: Aeson.Value -> Aeson.Value -> (TestEnvironment, ()) -> IO ()
setupFixture sourceMetadata backendConfig (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment sourceMetadata (Just backendConfig)

-- | Teardown the schema and tracking in the most expected way.
teardown :: (TestEnvironment, ()) -> IO ()
teardown (testEnvironment, _) = do
  GraphqlEngine.clearMetadata testEnvironment

--------------------------------------------------------------------------------
-- Mock Agent
--
-- Current Design:
--
-- The Mock Agent receives at startup a 'I.IORef MockConfig' and an
-- empty 'I.IORef (Maybe API.Query)'.
--
-- The 'MockConfig' contains static responses for all the Agent
-- endpoints. When the agent handlers are called, they read from the
-- 'I.IORef MockConfig' and return the value revelevant to the handler.
--
-- In the case of the Query Handler, before returning the mock value,
-- the handler writes the incoming 'API.Query' value to the 'I.IORef
-- (Maybe API.Query)'.
--
-- The two 'I.IORef' values are constructed when we build the local
-- test environment in 'mkLocalTestEnvironmentMock' and call
-- 'runMockServer'. We return '(I.IORef MockConfig, I.IORef (Maybe
-- API.Query), ThreadId)' so that we can use the 'I.IORef' values in
-- test setups and the 'ThreadId' in teardown (to kill the agent
-- thread).
--
-- NOTE: In the current design we use the same agent and the same
-- 'I.IORef's for all tests. This is safe because the tests are run
-- sequentially. Parallelizing the test suite would break the testing
-- setup for 'DataConnector'.
--
-- If a parallelization refactor occurs, we will need to construct a
-- mock agent and corresponding 'I.IORef's for each individual
-- test. To make this work we would likely need to use hspec hooks on
-- the individual tests to spawn and destroy a mock agent and
-- associated 'I.IORef's.

data MockAgentEnvironment = MockAgentEnvironment
  { maeConfig :: I.IORef MockConfig,
    maeQuery :: I.IORef (Maybe API.QueryRequest),
    maeThreadId :: ThreadId
  }

-- | Create the 'I.IORef's and launch the servant mock agent.
mkLocalTestEnvironmentMock :: TestEnvironment -> IO MockAgentEnvironment
mkLocalTestEnvironmentMock _ = do
  maeConfig <- I.newIORef chinookMock
  maeQuery <- I.newIORef Nothing
  maeThreadId <- forkIO $ runMockServer maeConfig maeQuery
  pure $ MockAgentEnvironment {..}

-- | Load the agent schema into HGE.
setupMock :: Aeson.Value -> Aeson.Value -> (TestEnvironment, MockAgentEnvironment) -> IO ()
setupMock sourceMetadata backendConfig (testEnvironment, _mockAgentEnvironment) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment sourceMetadata (Just backendConfig)

-- | Teardown the schema and kill the servant mock agent.
teardownMock :: (TestEnvironment, MockAgentEnvironment) -> IO ()
teardownMock (testEnvironment, MockAgentEnvironment {..}) = do
  GraphqlEngine.clearMetadata testEnvironment
  killThread maeThreadId

-- | Mock Agent test case input.
data TestCase = TestCase
  { -- | The Mock configuration for the agent
    _given :: MockConfig,
    -- | The Graphql Query to test
    _whenRequest :: Aeson.Value,
    -- | The expected HGE 'API.Query' value to be provided to the
    -- agent. A @Nothing@ value indicates that the 'API.Query'
    -- assertion should be skipped.
    _whenQuery :: Maybe API.QueryRequest,
    -- | The expected GQL response and outgoing HGE 'API.Query'
    _then :: Aeson.Value
  }

-- | Test runner for the Mock Agent. 'runMockedTest' sets the mocked
-- value in the agent, fires a GQL request, then asserts on the
-- expected response and 'API.Query' value.
runMockedTest :: Options -> TestCase -> (TestEnvironment, MockAgentEnvironment) -> IO ()
runMockedTest opts TestCase {..} (testEnvironment, MockAgentEnvironment {..}) = do
  -- Set the Agent with the 'MockConfig'
  I.writeIORef maeConfig _given

  -- Execute the GQL Query and assert on the result
  shouldReturnYaml
    opts
    ( GraphqlEngine.postGraphql
        testEnvironment
        _whenRequest
    )
    _then

  -- Read the logged 'API.QueryRequest' from the Agent
  query <- I.readIORef maeQuery
  I.writeIORef maeQuery Nothing

  -- Assert that the 'API.QueryRequest' was constructed how we expected.
  onJust _whenQuery ((query `shouldBe`) . Just)
