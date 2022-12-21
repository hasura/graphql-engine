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

import Control.Lens qualified as Lens
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as Vector
import Harness.Backend.DataConnector.Chinook (ChinookTestEnv)
import Harness.Backend.DataConnector.Chinook qualified as Chinook
import Harness.Backend.DataConnector.Chinook.Reference qualified as Reference
import Harness.Backend.DataConnector.Chinook.Sqlite qualified as Sqlite
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
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
            { name = Fixture.Backend Reference.backendTypeMetadata,
              mkLocalTestEnvironment = \_ -> pure $ Chinook.ChinookTestEnv Reference.sourceConfiguration id id id,
              setupTeardown = \(testEnv, _localEnv) ->
                [emptySetupAction testEnv],
              customOptions = Nothing
            },
          Fixture
            { name = Fixture.Backend Sqlite.backendTypeMetadata,
              mkLocalTestEnvironment = \_ -> pure $ Chinook.ChinookTestEnv Sqlite.sourceConfiguration id id Sqlite.formatForeignKeyName,
              setupTeardown = \(testEnv, _localEnv) ->
                [emptySetupAction testEnv],
              customOptions = Nothing
            }
        ]
    )
    schemaCrudTests

  Fixture.runWithLocalTestEnvironmentSingleSetup
    ( NE.fromList
        [ Fixture
            { name = Fixture.Backend Reference.backendTypeMetadata,
              mkLocalTestEnvironment = \_ -> pure $ Chinook.ChinookTestEnv Reference.sourceConfiguration id id id,
              setupTeardown = \(testEnv, _localEnv) ->
                [Chinook.setupAction Chinook.referenceSourceConfig Reference.agentConfig testEnv],
              customOptions = Nothing
            },
          Fixture
            { name = Fixture.Backend Sqlite.backendTypeMetadata,
              mkLocalTestEnvironment = \_ -> pure $ Chinook.ChinookTestEnv Sqlite.sourceConfiguration id id Sqlite.formatForeignKeyName,
              setupTeardown = \(testEnv, _localEnv) ->
                [Chinook.setupAction Chinook.sqliteSourceConfig Sqlite.agentConfig testEnv],
              customOptions = Nothing
            }
        ]
    )
    schemaInspectionTests

--------------------------------------------------------------------------------

schemaInspectionTests :: Fixture.Options -> SpecWith (TestEnvironment, ChinookTestEnv)
schemaInspectionTests opts = describe "Schema and Source Inspection" $ do
  describe "get_source_tables" $ do
    it "success" $ \(testEnvironment, Chinook.ChinookTestEnv {..}) -> do
      let sortYamlArray :: J.Value -> IO J.Value
          sortYamlArray (J.Array a) = pure $ J.Array (Vector.fromList (sort (Vector.toList a)))
          sortYamlArray _ = fail "Should return Array"

      case BackendType.backendSourceName <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend not found for testEnvironment"
        Just sourceString -> do
          let album = formatTableName ["Album"]
              artist = formatTableName ["Artist"]
              customer = formatTableName ["Customer"]
              employee = formatTableName ["Employee"]
              genre = formatTableName ["Genre"]
              invoice = formatTableName ["Invoice"]
              invoiceLine = formatTableName ["InvoiceLine"]
              mediaType = formatTableName ["MediaType"]
              playlist = formatTableName ["Playlist"]
              playlistTrack = formatTableName ["PlaylistTrack"]
              track = formatTableName ["Track"]
          shouldReturnYamlF
            sortYamlArray
            opts
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: get_source_tables
                args:
                  source: *sourceString
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
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {..}) -> do
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
          let album = formatTableName ["Album"]
              albumId = formatColumnName "AlbumId"
              artistId = formatColumnName "ArtistId"
              title = formatColumnName "Title"
              artistForeignKeys = formatForeignKeyName "Artist"
          shouldReturnYamlF
            (pure . removeDescriptions)
            opts
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
                type: number
                insertable: *supportsInserts
                updatable: *supportsUpdates
              - name: *artistId
                nullable: false
                type: number
                insertable: *supportsInserts
                updatable: *supportsUpdates
              - name: *title
                nullable: false
                type: string
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
                  foreign_table:
                    - Artist
                  column_mapping:
                    ArtistId: ArtistId
            |]
                & applyWhen (columnNullability == API.OnlyNullableColumns) (Lens.set (key "columns" . _Array . Lens.each . key "nullable") (J.Bool True))
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
            opts
            ( ( GraphqlEngine.postMetadata
                  testEnvironment
                  [yaml|
                type: get_source_kind_capabilities
                args:
                  name: *backendString
              |]
              ) -- Note: These fields are backend specific so we ignore their values and just verify their shapes:
                <&> Lens.set (key "config_schema_response" . key "other_schemas") J.Null
                <&> Lens.set (key "config_schema_response" . key "config_schema") J.Null
                <&> Lens.set (key "options" . key "uri") J.Null
                <&> Lens.set (_Object . Lens.at "display_name") (Just J.Null)
            )
            [yaml|
            capabilities: *backendCapabilities
            config_schema_response:
              config_schema: null
              other_schemas: null
            display_name: null
            options:
              uri: null
            |]

schemaCrudTests :: Fixture.Options -> SpecWith (TestEnvironment, ChinookTestEnv)
schemaCrudTests opts = describe "A series of actions to setup and teardown a source with tracked tables and relationships" $ do
  describe "dc_add_agent" $ do
    it "Success" $ \(testEnvironment, _) -> do
      case ( backendServerUrl =<< getBackendTypeConfig testEnvironment,
             backendTypeString <$> getBackendTypeConfig testEnvironment
           ) of
        (Nothing, _) -> pendingWith "Capabilities not found in testEnvironment"
        (_, Nothing) -> pendingWith "Backend Type not found in testEnvironment"
        (Just serverString, Just backendString) -> do
          shouldReturnYaml
            opts
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
      case (backendTypeString &&& backendDisplayNameString) <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendString, backendDisplayName) -> do
          shouldReturnYaml
            opts
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: list_source_kinds
                args: {}
              |]
            )
            [yaml|
              sources:
              - builtin: true
                kind: pg
                display_name: pg
              - builtin: true
                kind: citus
                display_name: citus
              - builtin: true
                kind: cockroach
                display_name: cockroach
              - builtin: true
                kind: mssql
                display_name: mssql
              - builtin: true
                kind: bigquery
                display_name: bigquery
              - builtin: true
                kind: mysql
                display_name: mysql
              - builtin: false
                kind: *backendString
                display_name: *backendDisplayName
              - builtin: false
                display_name: "FOOBARDB (foobar)"
                kind: foobar
            |]

  describe "<kind>_add_source" $ do
    it "success" $ \(testEnvironment, Chinook.ChinookTestEnv {..}) -> do
      let backendTypeMetadata = TestEnvironment.getBackendTypeConfig testEnvironment
      case (backendTypeString &&& backendSourceName) <$> backendTypeMetadata of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendTypeString, sourceName) -> do
          let actionType = backendTypeString <> "_add_source"
          shouldReturnYaml
            opts
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
    it "success" $ \(testEnvironment, Chinook.ChinookTestEnv {..}) -> do
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_track_table"
              album = formatTableName ["Album"]
          shouldReturnYaml
            opts
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

  describe "<kind>_create_object_relationship" $ do
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {..}) -> do
      let foreignKeySupport = (getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities) <&> API._dscSupportsForeignKeys . API._cDataSchema
      case (backendTypeString &&& backendSourceName) <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_track_table"
              artistTable = formatTableName ["Artist"]
              albumTable = formatTableName ["Album"]
              artistId = formatColumnName "ArtistId"
          GraphqlEngine.postMetadata_
            testEnvironment
            [yaml|
            type: *actionType
            args:
              source: *sourceName
              table: *artistTable
          |]

          when
            (foreignKeySupport == Just False)
            (pendingWith "Backend does not support Foreign Key constraints")

          let createObjectRelationAction = backendType <> "_create_object_relationship"
          shouldReturnYaml
            opts
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
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {..}) -> do
      let foreignKeySupport = (getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities) <&> API._dscSupportsForeignKeys . API._cDataSchema
      when
        (foreignKeySupport == Just False)
        (pendingWith "Backend does not support Foreign Key constraints")
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_create_array_relationship"
              albumTable = formatTableName ["Album"]
              artistTable = formatTableName ["Artist"]
          shouldReturnYaml
            opts
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
                        - ArtistId
              |]
            )
            [yaml|
              message: success
            |]

  describe "export_metadata" $ do
    it "produces the expected metadata structure" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {..}) -> do
      case ((fold . backendServerUrl) &&& backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (agentUrl, (backendType, sourceName)) -> do
          let foreignKeySupport = (getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities) <&> API._dscSupportsForeignKeys . API._cDataSchema
              albumTable = formatTableName ["Album"]
              artistTable = formatTableName ["Artist"]
          shouldReturnYaml
            opts
            ( GraphqlEngine.postMetadata
                testEnvironment
                [yaml|
                type: export_metadata
                args: {}
              |]
            )
            if foreignKeySupport == Just True
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
                        foreign_key_constraint_on: ArtistId
                    table: *albumTable
                  - array_relationships:
                    - name: Albums
                      using:
                        foreign_key_constraint_on:
                          column: ArtistId
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
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {..}) -> do
      let foreignKeySupport = (getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities) <&> API._dscSupportsForeignKeys . API._cDataSchema
      when
        (foreignKeySupport == Just False)
        (pendingWith "Backend does not support Foreign Key constraints")
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_drop_relationship"
              artistTable = formatTableName ["Artist"]
          shouldReturnYaml
            opts
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
    it "success" $ \(testEnvironment@TestEnvironment {}, Chinook.ChinookTestEnv {..}) -> do
      let foreignKeySupport = (getBackendTypeConfig testEnvironment >>= BackendType.parseCapabilities) <&> API._dscSupportsForeignKeys . API._cDataSchema
      when
        (foreignKeySupport == Just False)
        (pendingWith "Backend does not support Foreign Key constraints")
      case (backendTypeString &&& backendSourceName) <$> TestEnvironment.getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend Type not found in testEnvironment"
        Just (backendType, sourceName) -> do
          let actionType = backendType <> "_untrack_table"
              artistTable = formatTableName ["Artist"]
          shouldReturnYaml
            opts
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
            opts
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
            opts
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
