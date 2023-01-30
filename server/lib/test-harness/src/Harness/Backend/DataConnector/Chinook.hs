{-# LANGUAGE QuasiQuotes #-}

-- | Chinook Based Agent Fixtures used in DataConnector specific
-- Specs.
module Harness.Backend.DataConnector.Chinook
  ( setupAction,
    referenceSourceConfig,
    sqliteSourceConfig,
    testRoleName,
    ChinookTestEnv (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Harness.Backend.DataConnector.Chinook.Reference qualified as Reference
import Harness.Backend.DataConnector.Chinook.Sqlite qualified as Sqlite
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude

--------------------------------------------------------------------------------

data ChinookTestEnv = ChinookTestEnv
  { -- | Default configuration JSON for the backend source.
    backendSourceConfig :: Aeson.Value,
    -- | Can be used to apply custom formatting to table names. Eg.,
    -- adjusting the casing.
    formatTableName :: [Text] -> [Text],
    -- | Can be used to apply custom formatting to column names. Eg.,
    -- adjusting the casing.
    formatColumnName :: Text -> Text,
    formatForeignKeyName :: Text -> Text
  }

setupAction :: Aeson.Value -> Aeson.Value -> TestEnvironment -> Fixture.SetupAction
setupAction sourceMetadata backendConfig' testEnv =
  Fixture.SetupAction
    (setup sourceMetadata backendConfig' testEnv)
    (const $ teardown testEnv)

-- | Setup the schema given source metadata and backend config.
setup :: Aeson.Value -> Aeson.Value -> TestEnvironment -> IO ()
setup sourceMetadata backendConfig' testEnvironment = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment sourceMetadata (Just backendConfig')

-- | Teardown the schema and tracking in the most expected way.
teardown :: TestEnvironment -> IO ()
teardown testEnvironment = do
  GraphqlEngine.clearMetadata testEnvironment

--------------------------------------------------------------------------------

referenceSourceConfig :: Aeson.Value
referenceSourceConfig = mkChinookSourceConfig Reference.backendTypeMetadata Reference.sourceConfiguration

sqliteSourceConfig :: Aeson.Value
sqliteSourceConfig = mkChinookSourceConfig Sqlite.backendTypeMetadata Sqlite.sourceConfiguration

-- | Build a standard Chinook Source given an Agent specific @configuration@ field.
mkChinookSourceConfig :: Fixture.BackendTypeConfig -> Aeson.Value -> Aeson.Value
mkChinookSourceConfig backendTypeMetadata config =
  let source = Fixture.backendSourceName backendTypeMetadata
      backendTypeString = Fixture.backendTypeString backendTypeMetadata
   in [yaml|
name : *source
kind: *backendTypeString
tables:
  - table: [Album]
    object_relationships:
      - name: Artist
        using:
          manual_configuration:
            remote_table: [Artist]
            column_mapping:
              ArtistId: ArtistId
    select_permissions:
      - role: test-role
        permission:
          columns:
            - AlbumId
            - Title
            - ArtistId
          filter:
            _exists:
              _table: [Customer]
              _where:
                CustomerId:
                  _eq: X-Hasura-CustomerId
  - table: [Artist]
    array_relationships:
      - name: Albums
        using:
          manual_configuration:
            remote_table: [Album]
            column_mapping:
              ArtistId: ArtistId
  - table: [Playlist]
    array_relationships:
    - name : Tracks
      using:
        foreign_key_constraint_on:
          column: PlaylistId
          table:
          - PlaylistTrack
  - table: [PlaylistTrack]
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
  - table: [Track]
  - table: [Employee]
    array_relationships:
      - name: SupportRepForCustomers
        using:
          manual_configuration:
            remote_table: [Customer]
            column_mapping: 
              EmployeeId: SupportRepId
    select_permissions:
      - role: test-role
        permission:
          columns:
            - EmployeeId
            - FirstName
            - LastName
            - Country
          filter:
            SupportRepForCustomers:
              Country:
                _ceq: [ "$", "Country" ]
  - table: [Customer]
    object_relationships:
      - name: SupportRep
        using:
          manual_configuration:
            remote_table: [Employee]
            column_mapping:
              SupportRepId: EmployeeId
    select_permissions:
      - role: test-role
        permission:
          columns:
            - CustomerId
            - FirstName
            - LastName
            - Country
            - SupportRepId
          filter:
            SupportRep:
              Country:
                _ceq: [ "$", "Country" ]
      

configuration:
  *config
|]

-- | Dummy Role Name for testing.
testRoleName :: ByteString
testRoleName = "test-role"
