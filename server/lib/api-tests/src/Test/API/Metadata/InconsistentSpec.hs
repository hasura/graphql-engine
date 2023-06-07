{-# LANGUAGE QuasiQuotes #-}

module Test.API.Metadata.InconsistentSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postMetadata, postMetadata_)
import Harness.Quoter.Yaml
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment
import Harness.Yaml qualified as Yaml
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction [] testEnvironment,
                  dropTablesBeforeAndAfter testEnvironment,
                  setupMetadata testEnvironment
                ]
            }
        ]
    )
    tests

table :: Schema.Table
table = (Schema.table "table1") {Schema.tableColumns = [Schema.column "id" Schema.TInt]}

tests :: SpecWith TestEnvironment
tests = do
  describe "reloading the metadata" do
    it "flags missing tables as inconsistent" \testEnvironment -> do
      let shouldReturnYaml :: IO Value -> Value -> IO ()
          shouldReturnYaml = Yaml.shouldReturnYaml testEnvironment

      Postgres.createTable testEnvironment table

      postMetadata testEnvironment (replaceMetadataWithTable testEnvironment)
        `shouldReturnYaml` [yaml|
          is_consistent: true
          inconsistent_objects: []
        |]

      Postgres.dropTable testEnvironment table

      postMetadata testEnvironment reloadMetadata
        `shouldReturnYaml` expectedInconsistentYaml (Just "success") testEnvironment

    it "recognizes when tables have become consistent" \testEnvironment -> do
      let shouldReturnYaml :: IO Value -> Value -> IO ()
          shouldReturnYaml = Yaml.shouldReturnYaml testEnvironment

      postMetadata testEnvironment (replaceMetadataWithTable testEnvironment)
        `shouldReturnYaml` expectedInconsistentYaml Nothing testEnvironment

      Postgres.createTable testEnvironment table

      postMetadata testEnvironment reloadMetadata
        `shouldReturnYaml` [yaml|
          message: success
          is_consistent: true
        |]

      -- To be removed once the test fixtures can automatically clean this up.
      Postgres.dropTable testEnvironment table

  describe "replacing the metadata" do
    it "flags missing tables as inconsistent" \testEnvironment -> do
      let shouldReturnYaml :: IO Value -> Value -> IO ()
          shouldReturnYaml = Yaml.shouldReturnYaml testEnvironment

      postMetadata testEnvironment (replaceMetadataWithTable testEnvironment)
        `shouldReturnYaml` expectedInconsistentYaml Nothing testEnvironment

    it "recognizes when tables have become consistent" \testEnvironment -> do
      let shouldReturnYaml :: IO Value -> Value -> IO ()
          shouldReturnYaml = Yaml.shouldReturnYaml testEnvironment

      postMetadata testEnvironment (replaceMetadataWithTable testEnvironment)
        `shouldReturnYaml` expectedInconsistentYaml Nothing testEnvironment

      Postgres.createTable testEnvironment table

      postMetadata testEnvironment (replaceMetadataWithTable testEnvironment)
        `shouldReturnYaml` [yaml|
          is_consistent: true
          inconsistent_objects: []
        |]
      Postgres.dropTable testEnvironment table

  describe "replacing metadata with already present inconsistency" do
    it "drop table for an already inconsistent table with event trigger" \testEnvironment -> do
      let shouldReturnYaml :: IO Value -> Value -> IO ()
          shouldReturnYaml = Yaml.shouldReturnYaml testEnvironment

      Postgres.createTable testEnvironment table
      _ <- postMetadata testEnvironment (setupMetadataWithTableAndEventTrigger testEnvironment)
      Postgres.dropTable testEnvironment table
      postMetadata testEnvironment (replaceMetadataDropInconsistentTable testEnvironment)
        `shouldReturnYaml` [yaml|
          is_consistent: true
          inconsistent_objects: []
        |]

    it "drop source for an already inconsistent source" \testEnvironment -> do
      let shouldReturnYaml :: IO Value -> Value -> IO ()
          shouldReturnYaml = Yaml.shouldReturnYaml testEnvironment

      _ <- postMetadata testEnvironment (setupMetadataWithInconsistentSource testEnvironment)
      postMetadata testEnvironment repaceMetadataRemoveInconsistentSource
        `shouldReturnYaml` [yaml|
          is_consistent: true
          inconsistent_objects: []
        |]

reloadMetadata :: Value
reloadMetadata =
  [yaml|
    type: reload_metadata
    args:
      reload_sources: true
  |]

replaceMetadataWithTable :: TestEnvironment -> Value
replaceMetadataWithTable testEnvironment =
  [yaml|
    type: replace_metadata
    args:
      allow_inconsistent_metadata: true
      metadata:
        version: 3
        sources:
          - name: *sourceName
            kind: postgres
            tables:
              - table:
                  schema: *schemaName
                  name: *tableName
            configuration: *sourceConfiguration
  |]
  where
    backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    sourceConfiguration = Postgres.defaultSourceConfiguration testEnvironment
    sourceName = Fixture.backendSourceName backendTypeMetadata
    schemaName = Schema.getSchemaName testEnvironment
    tableName = Schema.tableName table

setupMetadataWithInconsistentSource :: TestEnvironment -> Value
setupMetadataWithInconsistentSource testEnvironment =
  [yaml|
    type: replace_metadata
    args:
      allow_inconsistent_metadata: true
      metadata:
        version: 3
        sources:
          - name: *sourceName
            kind: postgres
            tables:
              - table:
                  schema: *schemaName
                  name: *tableName
            configuration:
              connection_info:
                database_url: postgres://postgres:postgres@postgres:5432/non_existent_db
                pool_settings: {}
  |]
  where
    backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    sourceName = Fixture.backendSourceName backendTypeMetadata
    schemaName = Schema.getSchemaName testEnvironment
    tableName = Schema.tableName table

repaceMetadataRemoveInconsistentSource :: Value
repaceMetadataRemoveInconsistentSource =
  [yaml|
    type: replace_metadata
    args:
      allow_inconsistent_metadata: true
      metadata:
        version: 3
        sources: []
  |]

setupMetadataWithTableAndEventTrigger :: TestEnvironment -> Value
setupMetadataWithTableAndEventTrigger testEnvironment =
  [yaml|
    type: replace_metadata
    args:
      allow_inconsistent_metadata: true
      metadata:
        version: 3
        sources:
          - name: *sourceName
            kind: postgres
            tables:
              - table:
                  schema: *schemaName
                  name: *tableName
                event_triggers:
                  - name: foo-trigger
                    definition:
                      insert:
                        columns: '*'
                    retry_conf:
                      interval_sec: 10
                      num_retries: 0
                      timeout_sec: 60
                    webhook: https://httpbin.org/post
            configuration: *sourceConfiguration
  |]
  where
    backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    sourceConfiguration = Postgres.defaultSourceConfiguration testEnvironment
    sourceName = Fixture.backendSourceName backendTypeMetadata
    schemaName = Schema.getSchemaName testEnvironment
    tableName = Schema.tableName table

replaceMetadataDropInconsistentTable :: TestEnvironment -> Value
replaceMetadataDropInconsistentTable testEnvironment =
  [yaml|
    type: replace_metadata
    args:
      allow_inconsistent_metadata: true
      metadata:
        version: 3
        sources:
          - name: *sourceName
            kind: postgres
            tables: []
            configuration: *sourceConfiguration
  |]
  where
    backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    sourceConfiguration = Postgres.defaultSourceConfiguration testEnvironment
    sourceName = Fixture.backendSourceName backendTypeMetadata

expectedInconsistentYaml :: Maybe Text -> TestEnvironment -> Value
expectedInconsistentYaml message testEnvironment =
  [interpolateYaml|
    #{messageYaml}
    is_consistent: false
    inconsistent_objects:
      - definition:
          schema: #{schemaName}
          name: #{tableName}
        type: table
        name: "table #{schemaName}.#{tableName} in source #{sourceName}"
        reason: "Inconsistent object: no such table/view exists in source: \"#{schemaName}.#{tableName}\""
  |]
  where
    messageYaml = maybe "" ("message: " <>) message
    backendTypeMetadata = Maybe.fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    sourceName = Fixture.backendSourceName backendTypeMetadata
    schemaName = Schema.getSchemaName testEnvironment
    tableName = Schema.tableName table

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      sourceName = Fixture.backendSourceName backendTypeMetadata
      sourceConfiguration = Postgres.defaultSourceConfiguration testEnvironment

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [yaml|
            type: replace_metadata
            args:
              metadata:
                version: 3
                sources:
                  - name: *sourceName
                    kind: postgres
                    tables: []
                    configuration: *sourceConfiguration
          |]

      teardown :: IO ()
      teardown = setup

  Fixture.SetupAction setup \_ -> teardown

dropTablesBeforeAndAfter :: TestEnvironment -> Fixture.SetupAction
dropTablesBeforeAndAfter testEnvironment = do
  Fixture.SetupAction action (const action)
  where
    action = case BackendType.backendType <$> (getBackendTypeConfig testEnvironment) of
      Just Fixture.Postgres -> Postgres.dropTableIfExists testEnvironment table
      Just b -> fail $ "Unknown backend:" <> show b
      Nothing -> fail $ "Unknown backend."
