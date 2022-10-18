{-# LANGUAGE QuasiQuotes #-}

-- | Data Connector helpers.
module Harness.Backend.DataConnector
  ( -- * Fixture Setup
    TestSourceConfig (..),
    setupFixtureAction,
    setupFixture,
    teardown,

    -- * Sqlite Agent
    setupTablesAction,

    -- * Chinook Agents
    chinookConfigs,
    referenceBackendConfig,
    sqliteBackendConfig,
    chinookStockMetadata,

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
    setupMockAction,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock
import Harness.Backend.DataConnector.Sqlite
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture
  ( BackendType (DataConnectorMock, DataConnectorReference, DataConnectorSqlite),
    SetupAction (..),
    defaultBackendTypeString,
    defaultSource,
  )
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude

--------------------------------------------------------------------------------

data TestSourceConfig = TestSourceConfig
  { typeConfig :: BackendType,
    backendConfig :: Aeson.Value,
    sourceConfig :: Aeson.Value,
    metadataConfig :: Aeson.Value
  }
  deriving (Show, Eq)

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

--------------------------------------------------------------------------------
-- Agent Config

chinookConfigs :: NE.NonEmpty TestSourceConfig
chinookConfigs =
  TestSourceConfig DataConnectorReference referenceBackendConfig emptyConfig chinookStockMetadata
    NE.:| [TestSourceConfig DataConnectorSqlite sqliteBackendConfig sqliteChinookConfig chinookSqliteMetadata]

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

sqliteChinookConfig :: Aeson.Value
sqliteChinookConfig =
  [yaml|
db: "/db.chinook.sqlite"
|]

--------------------------------------------------------------------------------
-- Chinook Metadata

chinookStockMetadata :: Aeson.Value
chinookStockMetadata = chinookMetadata DataConnectorReference emptyConfig

chinookSqliteMetadata :: Aeson.Value
chinookSqliteMetadata = chinookMetadata DataConnectorSqlite sqliteChinookConfig

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
