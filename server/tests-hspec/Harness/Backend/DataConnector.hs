{-# LANGUAGE QuasiQuotes #-}

-- | Data Connector helpers.
module Harness.Backend.DataConnector
  ( -- * Reference Agent
    setupFixture,
    teardown,
    backendConfigs,
    referenceBackendConfig,
    sqliteBackendConfig,
    chinookStockMetadata,
    TestSourceConfig (..),

    -- * Mock Agent
    MockConfig (..),
    MockAgentEnvironment (..),
    TestCase (..),
    TestCaseRequired (..),
    defaultTestCase,
    mockBackendConfig,
    chinookMock,
    runMockedTest,
    mkLocalTestEnvironmentMock,
    setupMock,
    teardownMock,
    setupFixtureAction,
    setupMockAction,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Data.Aeson qualified as Aeson
import Data.IORef qualified as I
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.MockAgent
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http (RequestHeaders, healthCheck)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture (BackendType (DataConnectorMock, DataConnectorReference, DataConnectorSqlite), Options, SetupAction (..), defaultBackendTypeString, defaultSource)
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (shouldBe)

--------------------------------------------------------------------------------

data TestSourceConfig = TestSourceConfig
  { typeConfig :: BackendType,
    backendConfig :: Aeson.Value,
    sourceConfig :: Aeson.Value,
    metadataConfig :: Aeson.Value
  }
  deriving (Show, Eq)

backendConfigs :: NE.NonEmpty TestSourceConfig
backendConfigs =
  TestSourceConfig DataConnectorReference referenceBackendConfig emptyConfig chinookStockMetadata
    NE.:| [TestSourceConfig DataConnectorSqlite sqliteBackendConfig sqliteConfig chinookSqliteMetadata]

referenceBackendConfig :: Aeson.Value
referenceBackendConfig =
  let backendType = defaultBackendTypeString $ DataConnectorReference
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65005/"
|]

sqliteBackendConfig :: Aeson.Value
sqliteBackendConfig =
  let backendType = defaultBackendTypeString DataConnectorSqlite
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65007/"
|]

mockBackendConfig :: Aeson.Value
mockBackendConfig =
  let backendType = defaultBackendTypeString $ DataConnectorMock
      agentUri = "http://127.0.0.1:" <> show mockAgentPort <> "/"
   in [yaml|
dataconnector:
  *backendType:
    uri: *agentUri
|]

emptyConfig :: Aeson.Value
emptyConfig = [yaml| {} |]

sqliteConfig :: Aeson.Value
sqliteConfig =
  [yaml|
db: "/db.chinook.sqlite"
|]

--------------------------------------------------------------------------------
-- Chinook Agent

setupFixtureAction :: Aeson.Value -> Aeson.Value -> TestEnvironment -> SetupAction
setupFixtureAction sourceMetadata backendConfig testEnv =
  SetupAction
    (setupFixture sourceMetadata backendConfig (testEnv, ()))
    (const $ teardown (testEnv, ()))

-- | Setup the schema given source metadata and backend config.
setupFixture :: Aeson.Value -> Aeson.Value -> (TestEnvironment, ()) -> IO ()
setupFixture sourceMetadata backendConfig (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment sourceMetadata (Just backendConfig)

-- | Teardown the schema and tracking in the most expected way.
teardown :: (TestEnvironment, ()) -> IO ()
teardown (testEnvironment, _) = do
  GraphqlEngine.clearMetadata testEnvironment

chinookStockMetadata :: Aeson.Value
chinookStockMetadata = chinookMetadata DataConnectorReference emptyConfig

chinookSqliteMetadata :: Aeson.Value
chinookSqliteMetadata = chinookMetadata DataConnectorSqlite sqliteConfig

chinookMetadata :: BackendType -> Aeson.Value -> Aeson.Value
chinookMetadata backendType config =
  let source = defaultSource backendType
      backendTypeString = defaultBackendTypeString backendType
   in [yaml|
name : *source
kind: *backendTypeString
tables:
  - table: [Album]
    configuration:
      custom_root_fields:
        select: albums
        select_by_pk: albums_by_pk
      column_config:
        AlbumId:
          custom_name: id
        Title:
          custom_name: title
        ArtistId:
          custom_name: artist_id
    object_relationships:
      - name: artist
        using:
          manual_configuration:
            remote_table: [Artist]
            column_mapping:
              ArtistId: ArtistId
  - table: [Artist]
    configuration:
      custom_root_fields:
        select: artists
        select_by_pk: artists_by_pk
      column_config:
        ArtistId:
          custom_name: id
        Name:
          custom_name: name
    array_relationships:
      - name: albums
        using:
          manual_configuration:
            remote_table: [Album]
            column_mapping:
              ArtistId: ArtistId
  - table: Playlist
    array_relationships:
    - name : Tracks
      using:
        foreign_key_constraint_on:
          column: PlaylistId
          table:
          - PlaylistTrack
  - table: PlaylistTrack
    object_relationships:
      - name: Playlist
        using:
          foreign_key_constraint_on: PlaylistId
      - name: Track
        using:
          manual_configuration:
            remote_table: [Track]
            column_mapping:
              TrackId: TrackId
  - table: Track
  - table: Employee
    configuration:
      custom_root_fields:
        select: employees
        select_by_pk: employee_by_pk
      column_config:
        BirthDate:
          custom_name: birth_date
        LastName:
          custom_name: last_name
configuration:
  *config
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
    maeQuery :: I.IORef (Maybe API.QueryRequest),
    maeThread :: Async (),
    maeQueryConfig :: I.IORef (Maybe API.Config)
  }

-- | Create the 'I.IORef's and launch the servant mock agent.
mkLocalTestEnvironmentMock :: TestEnvironment -> IO MockAgentEnvironment
mkLocalTestEnvironmentMock _ = do
  maeConfig <- I.newIORef chinookMock
  maeQuery <- I.newIORef Nothing
  maeQueryConfig <- I.newIORef Nothing
  maeThread <- Async.async $ runMockServer maeConfig maeQuery maeQueryConfig
  healthCheck $ "http://127.0.0.1:" <> show mockAgentPort <> "/health"
  pure $ MockAgentEnvironment {..}

setupMockAction :: Aeson.Value -> Aeson.Value -> (TestEnvironment, MockAgentEnvironment) -> SetupAction
setupMockAction sourceMetadata backendConfig testEnv =
  SetupAction
    (setupMock sourceMetadata backendConfig testEnv)
    (const $ teardownMock testEnv)

-- | Load the agent schema into HGE.
setupMock :: Aeson.Value -> Aeson.Value -> (TestEnvironment, MockAgentEnvironment) -> IO ()
setupMock sourceMetadata backendConfig (testEnvironment, _mockAgentEnvironment) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment sourceMetadata (Just backendConfig)

-- | Teardown the schema and kill the servant mock agent.
teardownMock :: (TestEnvironment, MockAgentEnvironment) -> IO ()
teardownMock (testEnvironment, MockAgentEnvironment {..}) = do
  GraphqlEngine.clearMetadata testEnvironment
  Async.cancel maeThread

-- | Mock Agent test case input.
data TestCase = TestCase
  { -- | The Mock configuration for the agent
    _given :: MockConfig,
    -- | The Graphql Query to test
    _whenRequest :: Aeson.Value,
    -- | The headers to use on the Graphql Query request
    _whenRequestHeaders :: RequestHeaders,
    -- | The expected HGE 'API.Query' value to be provided to the
    -- agent. A @Nothing@ value indicates that the 'API.Query'
    -- assertion should be skipped.
    _whenQuery :: Maybe API.QueryRequest,
    -- | The expected HGE 'API.QueryHeaders' response and outgoing HGE 'API.QueryHeaders'
    _whenConfig :: Maybe API.Config,
    -- | The expected GQL response and outgoing HGE 'API.Query'
    _then :: Aeson.Value
  }

data TestCaseRequired = TestCaseRequired
  { -- | The Mock configuration for the agent
    _givenRequired :: MockConfig,
    -- | The Graphql Query to test
    _whenRequestRequired :: Aeson.Value,
    -- | The expected GQL response and outgoing HGE 'API.Query'
    _thenRequired :: Aeson.Value
  }

defaultTestCase :: TestCaseRequired -> TestCase
defaultTestCase TestCaseRequired {..} =
  TestCase
    { _given = _givenRequired,
      _whenRequest = _whenRequestRequired,
      _whenRequestHeaders = [],
      _whenQuery = Nothing,
      _whenConfig = Nothing,
      _then = _thenRequired
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
    ( GraphqlEngine.postGraphqlWithHeaders
        testEnvironment
        _whenRequestHeaders
        _whenRequest
    )
    _then

  -- Read the logged 'API.QueryRequest' from the Agent
  query <- I.readIORef maeQuery
  I.writeIORef maeQuery Nothing

  -- Read the logged 'API.Config' from the Agent
  queryConfig <- I.readIORef maeQueryConfig
  I.writeIORef maeQueryConfig Nothing

  -- Assert that the 'API.QueryRequest' was constructed how we expected.
  onJust _whenQuery ((query `shouldBe`) . Just)
  onJust _whenConfig ((queryConfig `shouldBe`) . Just)
