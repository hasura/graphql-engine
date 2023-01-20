{-# LANGUAGE QuasiQuotes #-}

-- | GDC Mock Agent Fixture and test helpers
module Harness.Backend.DataConnector.Mock
  ( -- * Mock Fixture
    backendTypeMetadata,
    setupAction,
    setup,
    teardown,
    agentConfig,
    mkLocalTestEnvironment,

    -- * Mock Test Construction
    MockConfig (..),
    MockAgentEnvironment (..),
    mockAgentPort,
    chinookMock,
    AgentRequest (..),
    MockRequestResults (..),
    mockQueryResponse,
    mockMutationResponse,
    mockAgentTest,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Data.Aeson qualified as Aeson
import Data.IORef qualified as I
import Harness.Backend.DataConnector.Mock.Server
import Harness.Exceptions (HasCallStack)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http (RequestHeaders, healthCheck)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.TestResource (AcquiredResource (..), Managed, mkTestResource)
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (Arg, Expectation, SpecWith, it)

--------------------------------------------------------------------------------

backendTypeMetadata :: BackendType.BackendTypeConfig
backendTypeMetadata =
  BackendType.BackendTypeConfig
    { backendType = BackendType.DataConnectorMock,
      backendSourceName = "mock",
      backendCapabilities = Nothing,
      backendTypeString = "mock",
      backendDisplayNameString = "mock",
      backendServerUrl = Just "http://localhost:65006",
      backendSchemaKeyword = "schema"
    }

--------------------------------------------------------------------------------

setupAction :: Aeson.Value -> Aeson.Value -> (TestEnvironment, MockAgentEnvironment) -> Fixture.SetupAction
setupAction sourceMetadata backendConfig testEnv =
  Fixture.SetupAction
    (setup sourceMetadata backendConfig testEnv)
    (const $ teardown testEnv)

-- | Load the agent schema into HGE.
setup :: Aeson.Value -> Aeson.Value -> (TestEnvironment, MockAgentEnvironment) -> IO ()
setup sourceMetadata backendConfig (testEnvironment, _mockAgentEnvironment) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment sourceMetadata (Just backendConfig)

-- | Teardown the schema and kill the servant mock agent.
teardown :: (TestEnvironment, MockAgentEnvironment) -> IO ()
teardown (testEnvironment, MockAgentEnvironment {..}) = do
  GraphqlEngine.clearMetadata testEnvironment
  Async.cancel maeThread

--------------------------------------------------------------------------------

-- | Mock Agent @backend_configs@ field
agentConfig :: Aeson.Value
agentConfig =
  let backendType = BackendType.backendTypeString backendTypeMetadata
      agentUri = "http://127.0.0.1:" <> show mockAgentPort <> "/"
   in [yaml|
        dataconnector:
          *backendType:
            uri: *agentUri
        |]

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
    maeRecordedRequest :: I.IORef (Maybe AgentRequest),
    maeThread :: Async (),
    maeRecordedRequestConfig :: I.IORef (Maybe API.Config)
  }

-- | Create the 'I.IORef's and launch the servant mock agent.
mkLocalTestEnvironment :: TestEnvironment -> Managed MockAgentEnvironment
mkLocalTestEnvironment _ = mkTestResource do
  maeConfig <- I.newIORef chinookMock
  maeRecordedRequest <- I.newIORef Nothing
  maeRecordedRequestConfig <- I.newIORef Nothing
  maeThread <- Async.async $ runMockServer maeConfig maeRecordedRequest maeRecordedRequestConfig
  pure $
    AcquiredResource
      { resourceValue = MockAgentEnvironment {..},
        waitForResource = healthCheck $ "http://127.0.0.1:" <> show mockAgentPort <> "/health",
        teardownResource = Async.cancel maeThread
      }

data MockRequestResults = MockRequestResults
  { _mrrGraphqlResponse :: Aeson.Value,
    _mrrRecordedRequest :: Maybe AgentRequest,
    _mrrRecordedRequestConfig :: Maybe API.Config
  }

mockQueryResponse :: API.QueryResponse -> MockConfig -> MockConfig
mockQueryResponse queryResponse mockConfig =
  mockConfig {_queryResponse = \_ -> Right queryResponse}

mockMutationResponse :: API.MutationResponse -> MockConfig -> MockConfig
mockMutationResponse mutationResponse mockConfig =
  mockConfig {_mutationResponse = \_ -> Right mutationResponse}

mockAgentTest :: HasCallStack => String -> ((MockConfig -> RequestHeaders -> Aeson.Value -> IO MockRequestResults) -> Expectation) -> SpecWith (Arg ((TestEnvironment, MockAgentEnvironment) -> Expectation))
mockAgentTest name testBody =
  it name $ \env -> testBody (postMockAgentGraphqlWithHeaders env)

postMockAgentGraphqlWithHeaders :: HasCallStack => (TestEnvironment, MockAgentEnvironment) -> MockConfig -> RequestHeaders -> Aeson.Value -> IO MockRequestResults
postMockAgentGraphqlWithHeaders (testEnvironment, MockAgentEnvironment {..}) mockConfig requestHeaders graphqlRequest = do
  -- Set the Agent with the 'MockConfig'
  I.writeIORef maeConfig mockConfig

  -- Reset recording state
  I.writeIORef maeRecordedRequest Nothing
  I.writeIORef maeRecordedRequestConfig Nothing

  -- Perform GraphQL request
  graphqlResponse <- GraphqlEngine.postGraphqlWithHeaders testEnvironment requestHeaders graphqlRequest

  -- Capture recordings
  recordedRequest <- I.readIORef maeRecordedRequest
  recordedRequestConfig <- I.readIORef maeRecordedRequestConfig

  pure $ MockRequestResults graphqlResponse recordedRequest recordedRequestConfig
