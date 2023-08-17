{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Metadata API tests for Data Connector Backend
--
-- NOTE: The test 'Spec' is broken up to support running subsets of
-- the 'Spec' for specific backends.
module Test.DataConnector.MetadataApiSpec
  ( spec,
    schemaCrudTests,
    schemaInspectionTests,
    emptySetupAction,
  )
where

--------------------------------------------------------------------------------

import Control.Lens ((^?), _Just)
import Control.Lens qualified as Lens
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as Vector
import Harness.Backend.DataConnector.Chinook (ChinookTestEnv, NameFormatting (..), ScalarTypes (..))
import Harness.Backend.DataConnector.Chinook qualified as Chinook
import Harness.Backend.DataConnector.Chinook.Reference qualified as Reference
import Harness.Backend.DataConnector.Chinook.Sqlite qualified as Sqlite
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml (interpolateYaml)
import Harness.Test.BackendType (BackendTypeConfig (..))
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture (Fixture (..))
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Harness.TestEnvironment qualified as TestEnvironment
import Harness.Yaml (shouldReturnYaml, shouldReturnYamlF)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, pendingWith)

--------------------------------------------------------------------------------
-- DataConnector Agent Query Tests

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.runWithLocalTestEnvironmentSingleSetup
    ( NE.fromList
        [ Fixture
            { name = Fixture.Backend Reference.backendTypeConfig,
              mkLocalTestEnvironment = Reference.mkChinookStaticTestEnvironment,
              setupTeardown = \(testEnv, _localEnv) ->
                [emptySetupAction testEnv],
              customOptions = Nothing
            },
          Fixture
            { name = Fixture.Backend Sqlite.backendTypeConfig,
              mkLocalTestEnvironment = Sqlite.mkChinookCloneTestEnvironment,
              setupTeardown = \(testEnv, _localEnv) ->
                [emptySetupAction testEnv],
              customOptions = Nothing
            }
        ]
    )
    schemaCrudTests

  Fixture.runWithLocalTestEnvironmentSingleSetup
    ( NE.fromList
        [ Reference.chinookFixture,
          Sqlite.chinookFixture
        ]
    )
    schemaInspectionTests

--------------------------------------------------------------------------------

schemaInspectionTests :: SpecWith (TestEnvironment, ChinookTestEnv)
schemaInspectionTests = describe "Schema and Source Inspection" $ do
  describe "get_source_tables" $ do
    it "success" $ \(testEnvironment, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}}) -> do
      let sortYamlArray :: J.Value -> IO J.Value
          sortYamlArray (J.Array a) = pure $ J.Array (Vector.fromList (sort (Vector.toList a)))
          sortYamlArray _ = fail "Should return Array"

          backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          backendType = BackendType.backendTypeString backendTypeMetadata

      case BackendType.backendSourceName <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend not found for testEnvironment"
        Just sourceString -> do
          let album = _nfFormatTableName ["Album"]
              artist = _nfFormatTableName ["Artist"]
              customer = _nfFormatTableName ["Customer"]
              employee = _nfFormatTableName ["Employee"]
              genre = _nfFormatTableName ["Genre"]
              invoice = _nfFormatTableName ["Invoice"]
              invoiceLine = _nfFormatTableName ["InvoiceLine"]
              mediaType = _nfFormatTableName ["MediaType"]
              playlist = _nfFormatTableName ["Playlist"]
              playlistTrack = _nfFormatTableName ["PlaylistTrack"]
              track = _nfFormatTableName ["Track"]
          shouldReturnYamlF
            testEnvironment
            sortYamlArray
            ( GraphqlEngine.postMetadata
                testEnvironment
                [interpolateYaml|
                type: #{backendType}_get_source_tables
                args:
                  source: #{sourceString}
              |]
            )
            [yaml|
              - *album
              - *artist
              - *customer
              - *employee
              - *genre
              - *invoice
              - *invoiceLine
              - *mediaType
              - *playlist
              - *playlistTrack
              - *track
            |]

  describe "get_table_info" $ do
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}, scalarTypes = ScalarTypes {..}}) -> do
      let removeDescriptions (J.Object o) = J.Object (KM.delete "description" (removeDescriptions <$> o))
          removeDescriptions (J.Array a) = J.Array (removeDescriptions <$> a)
          removeDescriptions x = x
      let mutationsCapabilities =
            getBackendTypeConfig testEnvironment
              >>= BackendType.parseCapabilities
              >>= API._cMutations
      let supportsInserts = isJust $ mutationsCapabilities >>= API._mcInsertCapabilities
      let supportsUpdates = isJust $ mutationsCapabilities >>= API._mcUpdateCapabilities
      let supportsDeletes = isJust $ mutationsCapabilities >>= API._mcDeleteCapabilities
      let dataSchema = (getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities) <&> API._cDataSchema
      let supportsPrimaryKeys = any API._dscSupportsPrimaryKeys $ dataSchema
      let supportsForeignKeys = any API._dscSupportsForeignKeys $ dataSchema
      let columnNullability = maybe API.NullableAndNonNullableColumns API._dscColumnNullability dataSchema

      case BackendType.backendSourceName <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend not found for testEnvironment"
        Just sourceString -> do
          let album = _nfFormatTableName ["Album"]
              artist = _nfFormatTableName ["Artist"]
              albumId = _nfFormatColumnName "AlbumId"
              artistId = _nfFormatColumnName "ArtistId"
              title = _nfFormatColumnName "Title"
              artistForeignKeys = _nfFormatForeignKeyName "Artist"
          shouldReturnYamlF
            testEnvironment
            (pure . removeDescriptions)
            ( ( GraphqlEngine.postMetadata
                  testEnvironment
                  [yaml|
                  type: get_table_info
                  args:
                    source: *sourceString
                    table: *album
                |]
              )
                <&> Lens.over (key "columns" . _Array) (Vector.fromList . sortOn (Lens.preview (key "name")) . Vector.toList)
                <&> Lens.over (atKey "primary_key") (maybe Nothing (\value -> bool Nothing (Just value) supportsPrimaryKeys))
                <&> Lens.over (atKey "foreign_keys") (maybe Nothing (\value -> bool Nothing (Just value) supportsForeignKeys))
            )
            ( [yaml|
              columns:
              - name: *albumId
                nullable: false
                type: *_stIntegerType
                insertable: *supportsInserts
                updatable: *supportsUpdates
                value_generated:
                  type: auto_increment
              - name: *artistId
                nullable: false
                type: *_stIntegerType
                insertable: *supportsInserts
                updatable: *supportsUpdates
              - name: *title
                nullable: false
                type: *_stStringType
                insertable: *supportsInserts
                updatable: *supportsUpdates
              name: *album
              type: table
              primary_key:
              - *albumId
              insertable: *supportsInserts
              updatable: *supportsUpdates
              deletable: *supportsDeletes
              foreign_keys:
                *artistForeignKeys:
                  foreign_table: *artist
                  column_mapping:
                    *artistId: *artistId
            |]
                & applyWhen (columnNullability == API.OnlyNullableColumns) (Lens.set (key "columns" . _Array . Lens.each . key "nullable") (J.Bool True))
                & applyWhen (isNothing mutationsCapabilities) (Lens.set (key "columns" . _Array . Lens.each . _Object . Lens.at "value_generated") Nothing)
                & Lens.over (atKey "primary_key") (maybe Nothing (\value -> bool Nothing (Just value) supportsPrimaryKeys))
                & Lens.over (atKey "foreign_keys") (maybe Nothing (\value -> bool Nothing (Just value) supportsForeignKeys))
            )

  describe "get_source_kind_capabilities" $ do
    it "success" $ \(testEnvironment, _) -> do
      case ( BackendType.backendCapabilities =<< getBackendTypeConfig testEnvironment,
             BackendType.backendTypeString <$> getBackendTypeConfig testEnvironment
           ) of
        (Nothing, _) -> pendingWith "Capabilities not found in testEnvironment"
        (_, Nothing) -> pendingWith "Backend Type not found in testEnvironment"
        (Just backendCapabilities, Just backendString) -> do
          shouldReturnYaml
            testEnvironment
            ( ( GraphqlEngine.postMetadata
                  testEnvironment
                  [yaml|
                type: get_source_kind_capabilities
                args:
                  name: *backendString
              |]
              )
                -- Note: These fields are backend specific so we ignore their values and just verify their shapes:
                <&> Lens.set (key "config_schema_response" . key "other_schemas") J.Null
                <&> Lens.set (key "config_schema_response" . key "config_schema") J.Null
                <&> Lens.set (key "capabilities" . _Object . Lens.at "datasets") Nothing
                <&> Lens.set (key "capabilities" . _Object . Lens.at "licensing") Nothing
                <&> Lens.set (key "capabilities" . _Object . Lens.at "interpolated_queries") Nothing
                <&> Lens.set (key "capabilities" . key "queries" . _Object . Lens.at "redaction") Nothing
                <&> Lens.set (key "options" . key "uri") J.Null
                <&> Lens.set (_Object . Lens.at "display_name") Nothing
                <&> Lens.set (_Object . Lens.at "release_name") Nothing
            )
            [yaml|
            capabilities: *backendCapabilities
            config_schema_response:
              config_schema: null
              other_schemas: null
            options:
              uri: null
            |]

schemaCrudTests :: SpecWith (TestEnvironment, ChinookTestEnv)
schemaCrudTests = describe "A series of actions to setup and teardown a source with tracked tables and relationships" $ do
  describe "dc_add_agent" $ do
    it "Success" $ \(testEnvironment, _) -> do
      case ( backendServerUrl =<< getBackendTypeConfig testEnvironment,
             backendTypeString <$> getBackendTypeConfig testEnvironment
           ) of
        (Nothing, _) -> pendingWith "Capabilities not found in testEnvironment"
        (_, Nothing) -> pendingWith "Backend Type not found in testEnvironment"
        (Just serverString, Just backendString) -> do
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: dc_add_agent
                args:
                  name: *backendString
                  url: *serverString
              |]
            )
            [yaml|
              message: success
            |]

  describe "list_source_kinds" $ do
    it "success" $ \(testEnvironment, _) -> do
      case (backendTypeString &&& backendDisplayNameString &&& backendReleaseNameString) <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendString, (backendDisplayName, backendReleaseName)) -> do
          let dataConnectorSource =
                if isJust backendReleaseName
                  then
                    [yaml|
                    builtin: false
                    kind: *backendString
                    display_name: *backendDisplayName
                    release_name: *backendReleaseName
                    available: true
                  |]
                  else
                    [yaml|
                    builtin: false
                    kind: *backendString
                    display_name: *backendDisplayName
                    available: true
                  |]
          let sortSources =
                Lens.over
                  (key "sources" . _Array)
                  (Vector.fromList . sortOn (Lens.^.. key "kind") . Vector.toList)
          shouldReturnYaml
            testEnvironment
            ( sortSources
                <$> GraphqlEngine.postMetadata
                  testEnvironment
                  [yaml|
                  type: list_source_kinds
                  args: {}
                |]
            )
            ( sortSources
                [yaml|
                  sources:
                  - builtin: true
                    kind: pg
                    display_name: pg
                    available: true
                  - builtin: true
                    kind: citus
                    display_name: citus
                    available: true
                  - builtin: true
                    kind: cockroach
                    display_name: cockroach
                    available: true
                  - builtin: true
                    kind: mssql
                    display_name: mssql
                    available: true
                  - builtin: true
                    kind: bigquery
                    display_name: bigquery
                    available: true
                  - builtin: false
                    display_name: "FOOBARDB"
                    kind: foobar
                    available: true
                  - *dataConnectorSource
                |]
            )

  describe "<kind>_add_source" $ do
    it "success" $ \(testEnvironment, Chinook.ChinookTestEnv {..}) -> do
      let backendTypeMetadata = TestEnvironment.getBackendTypeConfig testEnvironment
      case (backendTypeString &&& backendSourceName) <$> backendTypeMetadata of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendTypeString, sourceName) -> do
          let actionType = backendTypeString <> "_add_source"
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *actionType
                args:
                  name: *sourceName
                  configuration: *backendSourceConfig
              |]
            )
            [yaml|
              message: success
            |]

  describe "<kind>_track_table" $ do
    it "success - track Album" $ \(testEnvironment, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}}) -> do
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_track_table"
              album = _nfFormatTableName ["Album"]
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *actionType
                args:
                  source: *sourceName
                  table: *album
              |]
            )
            [yaml|
              message: success
            |]

    it "success - track Artist" $ \(testEnvironment, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}}) -> do
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_track_table"
              artist = _nfFormatTableName ["Artist"]
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *actionType
                args:
                  source: *sourceName
                  table: *artist
              |]
            )
            [yaml|
              message: success
            |]

  describe "<kind>_create_object_relationship" $ do
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}}) -> do
      let capabilities = getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities
      let foreignKeySupport = fromMaybe False $ capabilities ^? _Just . API.cDataSchema . API.dscSupportsForeignKeys
      let relationshipsSupport = isJust $ capabilities ^? _Just . API.cRelationships . _Just
      unless relationshipsSupport
        $ pendingWith "Backend does not support local relationships"
      unless foreignKeySupport
        $ pendingWith "Backend does not support Foreign Key constraints"

      case (backendTypeString &&& backendSourceName) <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let albumTable = _nfFormatTableName ["Album"]
              artistId = _nfFormatColumnName "ArtistId"

          let createObjectRelationAction = backendType <> "_create_object_relationship"
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *createObjectRelationAction
                args:
                  source: *sourceName
                  table: *albumTable
                  name: Artist
                  using:
                    foreign_key_constraint_on:
                      - *artistId
              |]
            )
            [yaml|
              message: success
            |]

  describe "<kind>_create_array_relationship" $ do
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}}) -> do
      let capabilities = getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities
      let foreignKeySupport = fromMaybe False $ capabilities ^? _Just . API.cDataSchema . API.dscSupportsForeignKeys
      let relationshipsSupport = isJust $ capabilities ^? _Just . API.cRelationships . _Just
      unless relationshipsSupport
        $ pendingWith "Backend does not support local relationships"
      unless foreignKeySupport
        $ pendingWith "Backend does not support Foreign Key constraints"

      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_create_array_relationship"
              albumTable = _nfFormatTableName ["Album"]
              artistTable = _nfFormatTableName ["Artist"]
              artistId = _nfFormatColumnName "ArtistId"
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *actionType
                args:
                  source: *sourceName
                  table: *artistTable
                  name: Albums
                  using:
                    foreign_key_constraint_on:
                      table: *albumTable
                      columns:
                        - *artistId
              |]
            )
            [yaml|
              message: success
            |]

  describe "export_metadata" $ do
    it "produces the expected metadata structure" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}, ..}) -> do
      case ((fold . backendServerUrl) &&& backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (agentUrl, (backendType, sourceName)) -> do
          let capabilities = getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities
          let foreignKeySupport = fromMaybe False $ capabilities ^? _Just . API.cDataSchema . API.dscSupportsForeignKeys
          let relationshipsSupport = isJust $ capabilities ^? _Just . API.cRelationships . _Just
              albumTable = _nfFormatTableName ["Album"]
              artistTable = _nfFormatTableName ["Artist"]
              artistId = _nfFormatColumnName "ArtistId"
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: export_metadata
                args: {}
              |]
            )
            if foreignKeySupport && relationshipsSupport
              then
                [yaml|
                backend_configs:
                  dataconnector:
                    *backendType:
                      uri: *agentUrl
                sources:
                - configuration: *backendSourceConfig
                  kind: *backendType
                  name: *sourceName
                  tables:
                  - object_relationships:
                    - name: Artist
                      using:
                        foreign_key_constraint_on: *artistId
                    table: *albumTable
                  - array_relationships:
                    - name: Albums
                      using:
                        foreign_key_constraint_on:
                          column: *artistId
                          table: *albumTable
                    table: *artistTable
                version: 3
              |]
              else
                [yaml|
                backend_configs:
                  dataconnector:
                    *backendType:
                      uri: *agentUrl
                sources:
                - configuration: *backendSourceConfig
                  kind: *backendType
                  name: *sourceName
                  tables:
                  - table: *albumTable
                  - table: *artistTable
                version: 3
              |]

  describe "<kind>_drop_relationship" $ do
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}}) -> do
      let capabilities = getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities
      let foreignKeySupport = fromMaybe False $ capabilities ^? _Just . API.cDataSchema . API.dscSupportsForeignKeys
      let relationshipsSupport = isJust $ capabilities ^? _Just . API.cRelationships . _Just
      unless relationshipsSupport
        $ pendingWith "Backend does not support local relationships"
      unless foreignKeySupport
        $ pendingWith "Backend does not support Foreign Key constraints"

      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_drop_relationship"
              artistTable = _nfFormatTableName ["Artist"]
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *actionType
                args:
                  source: *sourceName
                  table: *artistTable
                  relationship: Albums
              |]
            )
            [yaml|
              message: success
            |]

  describe "<kind>_untrack_table" $ do
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {nameFormatting = NameFormatting {..}}) -> do
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_untrack_table"
              artistTable = _nfFormatTableName ["Artist"]
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *actionType
                args:
                  source: *sourceName
                  table: *artistTable
                  cascade: true
              |]
            )
            [yaml|
              message: success
            |]

  describe "<kind>_drop_source" $ do
    it "success" $ \(testEnvironment, _) -> do
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_drop_source"
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: *actionType
                args:
                  name: *sourceName
                  cascade: true
              |]
            )
            [yaml|
              message: success
            |]

  describe "dc_delete_agent" $ do
    it "success" $ \(testEnvironment, _) -> do
      case BackendType.backendTypeString <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just backendString -> do
          shouldReturnYaml
            testEnvironment
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: dc_delete_agent
                args:
                  name: *backendString
              |]
            )
            [yaml|
              message: success
            |]

-- | Setup a test action without any initialization then reset the
-- metadata in the teardown. This is useful for running tests on the Metadata API.
emptySetupAction :: TestEnvironment -> Fixture.SetupAction
emptySetupAction testEnvironment =
  Fixture.SetupAction
    { setupAction = pure (),
      teardownAction = const $ GraphqlEngine.clearMetadata testEnvironment
    }
