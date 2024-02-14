{-# LANGUAGE QuasiQuotes #-}

-- | GDC Mock Agent Fixture and test helpers
module Harness.Backend.DataConnector.Mock
  ( -- * Mock Fixture
    backendTypeMetadata,
    setupAction,
    setup,
    teardown,
    agentConfig,
    withMockAgent,
    mkLocalTestEnvironment,
    mkLocalTestEnvironment',

    -- * Mock Test Construction
    MockConfig (..),
    MockRequestConfig (..),
    MockAgentEnvironment (..),
    mockAgentPort,
    chinookMock,
    defaultMockRequestConfig,
    AgentRequest (..),
    MockRequestResults (..),
    mockQueryResponse,
    mockMutationResponse,
    mockAgentGraphqlTest,
    mockAgentMetadataTest,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Monad.Managed (runManaged)
import Data.Aeson qualified as J
import Data.Has
import Data.IORef qualified as I
import Harness.Backend.DataConnector.Mock.Server
import Harness.Exceptions (HasCallStack)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http (RequestHeaders, healthCheck)
import Harness.Quoter.Yaml (yaml)
import Harness.Services.GraphqlEngine (PostGraphql, PostMetadata)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.TestResource (AcquiredResource (..), Managed, mkTestResource)
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (Arg, Expectation, SpecWith, aroundWith, it)

--------------------------------------------------------------------------------

backendTypeMetadata :: BackendType.BackendTypeConfig
backendTypeMetadata =
  BackendType.BackendTypeConfig
    { backendType = BackendType.DataConnectorMock,
      backendSourceName = "mock",
      backendCapabilities = Nothing,
      backendTypeString = "mock",
      backendDisplayNameString = "mock",
      backendReleaseNameString = Nothing,
      backendServerUrl = Just "http://localhost:65006",
      backendSchemaKeyword = "schema",
      backendScalarType = const "",
      backendGraphQLType = const ""
    }

--------------------------------------------------------------------------------

setupAction :: J.Value -> J.Value -> (TestEnvironment, MockAgentEnvironment) -> Fixture.SetupAction
setupAction sourceMetadata backendConfig testEnv =
  Fixture.SetupAction
    (setup sourceMetadata backendConfig testEnv)
    (const $ teardown testEnv)

-- | Load the agent schema into HGE.
setup :: J.Value -> J.Value -> (TestEnvironment, MockAgentEnvironment) -> IO ()
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
agentConfig :: J.Value
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

withMockAgent ::
  SpecWith (testEnvironment, MockAgentEnvironment) ->
  SpecWith testEnvironment
withMockAgent specs = do
  flip aroundWith specs \action testEnvironment -> runManaged do
    agentEnv <- mkLocalTestEnvironment' chinookMock testEnvironment
    liftIO
      $ action (testEnvironment, agentEnv)

-- | Create the 'I.IORef's and launch the servant mock agent.
mkLocalTestEnvironment :: testEnvironment -> Managed MockAgentEnvironment
mkLocalTestEnvironment = mkLocalTestEnvironment' chinookMock

mkLocalTestEnvironment' :: MockConfig -> testEnvironment -> Managed MockAgentEnvironment
mkLocalTestEnvironment' mockConfig _ = mkTestResource do
  maeConfig <- I.newIORef mockConfig
  maeRecordedRequest <- I.newIORef Nothing
  maeRecordedRequestConfig <- I.newIORef Nothing
  maeThread <- Async.async $ runMockServer maeConfig maeRecordedRequest maeRecordedRequestConfig
  pure
    $ AcquiredResource
      { resourceValue = MockAgentEnvironment {..},
        waitForResource = healthCheck $ "http://127.0.0.1:" <> show mockAgentPort <> "/health",
        teardownResource = Async.cancel maeThread
      }

data MockRequestResults = MockRequestResults
  { _mrrResponse :: J.Value,
    _mrrRecordedRequest :: Maybe AgentRequest,
    _mrrRecordedRequestConfig :: Maybe API.Config
  }

mockQueryResponse :: API.QueryResponse -> MockRequestConfig
mockQueryResponse queryResponse =
  defaultMockRequestConfig {_queryResponse = \_ -> Right queryResponse}

mockMutationResponse :: API.MutationResponse -> MockRequestConfig
mockMutationResponse mutationResponse =
  defaultMockRequestConfig {_mutationResponse = \_ -> Right mutationResponse}

mockAgentGraphqlTest :: (HasCallStack, Has PostGraphql testEnvironment) => String -> (testEnvironment -> (MockRequestConfig -> RequestHeaders -> J.Value -> IO MockRequestResults) -> Expectation) -> SpecWith (Arg ((testEnvironment, MockAgentEnvironment) -> Expectation))
mockAgentGraphqlTest name testBody =
  it name $ \(env, agentEnv) ->
    let performGraphqlRequest mockRequestConfig requestHeaders graphqlRequest = performRecordedRequest agentEnv mockRequestConfig (GraphqlEngine.postGraphqlWithReqHeaders env requestHeaders graphqlRequest)
     in testBody env performGraphqlRequest

mockAgentMetadataTest :: (HasCallStack, Has PostMetadata testEnvironment) => String -> (testEnvironment -> (MockRequestConfig -> Int -> J.Value -> IO MockRequestResults) -> Expectation) -> SpecWith (Arg ((testEnvironment, MockAgentEnvironment) -> Expectation))
mockAgentMetadataTest name testBody =
  it name $ \(env, agentEnv) ->
    let performMetadataRequest mockRequestConfig status metadataRequest = performRecordedRequest agentEnv mockRequestConfig (GraphqlEngine.postMetadataWithStatusAndReqHeaders env status [] metadataRequest)
     in testBody env performMetadataRequest

performRecordedRequest :: (HasCallStack) => MockAgentEnvironment -> MockRequestConfig -> IO J.Value -> IO MockRequestResults
performRecordedRequest MockAgentEnvironment {..} mockRequestConfig performRequest = do
  -- Set the Agent with the 'MockConfig'
  I.modifyIORef maeConfig (\mockConfig -> mockConfig {_requestConfig = mockRequestConfig})

  -- Reset recording state
  I.writeIORef maeRecordedRequest Nothing
  I.writeIORef maeRecordedRequestConfig Nothing

  -- Perform GraphQL request
  response <- performRequest

  -- Capture recordings
  recordedRequest <- I.readIORef maeRecordedRequest
  recordedRequestConfig <- I.readIORef maeRecordedRequestConfig

  pure $ MockRequestResults response recordedRequest recordedRequestConfig
