{-# LANGUAGE QuasiQuotes #-}

-- | Chinook Based Agent Fixtures used in DataConnector specific
-- Specs.
module Harness.Backend.DataConnector.Chinook
  ( ChinookTestEnv (..),
    NameFormatting (..),
    ScalarTypes (..),
    mkChinookCloneTestEnvironment,
    mkChinookStaticTestEnvironment,
    setupChinookSourceAction,
    setupCustomSourceAction,
    testRoleName,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Managed (Managed)
import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Harness.DataConnectorAgent (createManagedClone)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment, getBackendTypeConfig)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude

--------------------------------------------------------------------------------

data ChinookTestEnv = ChinookTestEnv
  { -- | Default configuration JSON for the backend source.
    backendSourceConfig :: J.Value,
    -- | Default configuration for the backend config that sets the agent configuration
    backendAgentConfig :: J.Value,
    -- | Name formatting functions to correct for backend-specific naming rules
    nameFormatting :: NameFormatting,
    -- | Backend-specific expected scalar types
    scalarTypes :: ScalarTypes
  }

data NameFormatting = NameFormatting
  { -- | Can be used to apply custom formatting to table names. Eg.,
    -- adjusting the casing.
    _nfFormatTableName :: [Text] -> [Text],
    -- | Can be used to apply custom formatting to column names. Eg.,
    -- adjusting the casing.
    _nfFormatColumnName :: Text -> Text,
    _nfFormatForeignKeyName :: Text -> Text
  }

data ScalarTypes = ScalarTypes
  { _stFloatType :: Text,
    _stIntegerType :: Text,
    _stStringType :: Text
  }

--------------------------------------------------------------------------------

-- | Create a test environment that uses agent dataset cloning to clone a copy of the Chinook
-- DB for the test and use that as the source config configured in HGE.
-- This should be used with agents that support datasets.
mkChinookCloneTestEnvironment :: NameFormatting -> ScalarTypes -> TestEnvironment -> Managed ChinookTestEnv
mkChinookCloneTestEnvironment nameFormatting scalarTypes testEnv = do
  backendTypeConfig <- getBackendTypeConfig testEnv `onNothing` fail "Unable to find backend type config in this test environment"
  agentUrl <- Fixture.backendServerUrl backendTypeConfig `onNothing` fail ("Backend " <> show (Fixture.backendType backendTypeConfig) <> " does not have a server url")
  cloneConfig <- API._dccrConfig <$> createManagedClone agentUrl testEnv (API.DatasetTemplateName "Chinook")
  let agentBackendConfig = mkAgentBackendConfig backendTypeConfig
  let cloneConfigValue = J.Object $ API.unConfig cloneConfig
  let sourceConfig =
        [yaml|
          value: *cloneConfigValue
          template:
          timeout:
        |]
  pure $ ChinookTestEnv sourceConfig agentBackendConfig nameFormatting scalarTypes

-- | Create a test environment that uses the source config specified to connect to a specific DB on the agent
-- that contains the Chinook dataset.
-- This should be used with agents that do not support datasets.
mkChinookStaticTestEnvironment :: NameFormatting -> ScalarTypes -> J.Value -> TestEnvironment -> Managed ChinookTestEnv
mkChinookStaticTestEnvironment nameFormatting scalarTypes sourceConfig testEnv = do
  backendTypeConfig <- getBackendTypeConfig testEnv `onNothing` fail "Unable to find backend type config in this test environment"
  let agentBackendConfig = mkAgentBackendConfig backendTypeConfig
  pure $ ChinookTestEnv sourceConfig agentBackendConfig nameFormatting scalarTypes

--------------------------------------------------------------------------------

-- | Sets up a source in HGE using the source returned by 'mkSourceMetadata'
setupCustomSourceAction ::
  -- | Function that makes a source metadata, taking the 'BackendTypeConfig' and source's configuration as its input parameters
  (Fixture.BackendTypeConfig -> J.Value -> J.Value) ->
  (TestEnvironment, ChinookTestEnv) ->
  Fixture.SetupAction
setupCustomSourceAction mkSourceMetadata testEnvs@(testEnv, _chinookTestEnv) =
  Fixture.SetupAction
    (setupCustomSource mkSourceMetadata testEnvs)
    (const $ teardown testEnv)

-- | Sets up a source in HGE that contains tracked Chinook tables (see 'mkChinookSourceMetadata')
setupChinookSourceAction :: (TestEnvironment, ChinookTestEnv) -> Fixture.SetupAction
setupChinookSourceAction = setupCustomSourceAction mkChinookSourceMetadata

setupCustomSource ::
  -- | Function that makes a source metadata, taking the 'BackendTypeConfig' and source's configuration as its input parameters
  (Fixture.BackendTypeConfig -> J.Value -> J.Value) ->
  (TestEnvironment, ChinookTestEnv) ->
  IO ()
setupCustomSource mkSourceMetadata (testEnv, ChinookTestEnv {..}) = do
  backendTypeConfig <- getBackendTypeConfig testEnv `onNothing` fail "Unable to find backend type config in this test environment"
  let sourceMetadata = mkSourceMetadata backendTypeConfig backendSourceConfig
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnv sourceMetadata (Just backendAgentConfig)

-- | Teardown the schema and tracking in the most expected way.
teardown :: TestEnvironment -> IO ()
teardown testEnvironment = do
  GraphqlEngine.clearMetadata testEnvironment

--------------------------------------------------------------------------------

mkAgentBackendConfig :: Fixture.BackendTypeConfig -> J.Value
mkAgentBackendConfig backendTypeConfig =
  let backendType = Fixture.backendTypeString backendTypeConfig
      uri = Fixture.backendServerUrl backendTypeConfig
   in [yaml|
          dataconnector:
            *backendType:
              uri: *uri
          |]

--------------------------------------------------------------------------------

-- | Build a standard Chinook Source given an Agent specific @configuration@ field.
mkChinookSourceMetadata :: Fixture.BackendTypeConfig -> J.Value -> J.Value
mkChinookSourceMetadata backendTypeMetadata config =
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
  - table: [Invoice]
    array_relationships:
      - name: InvoiceLines
        using:
          manual_configuration:
            remote_table: [InvoiceLine]
            column_mapping:
              InvoiceId: InvoiceId
  - table: [InvoiceLine]
    object_relationships:
      - name: Invoice
        using:
          manual_configuration:
            remote_table: [Invoice]
            column_mapping:
              InvoiceId: InvoiceId

configuration:
  *config
|]

-- | Dummy Role Name for testing.
testRoleName :: ByteString
testRoleName = "test-role"
