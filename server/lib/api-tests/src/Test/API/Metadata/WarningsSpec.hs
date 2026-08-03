{-# LANGUAGE QuasiQuotes #-}

module Test.API.Metadata.WarningsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postMetadataWithStatus, postMetadata_)
import Harness.Quoter.Yaml
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment
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

-- | NOTE: these no longer test warnings. The current state represents a shift
-- in behavior from warnings to errors for the cases below
tests :: SpecWith TestEnvironment
tests = do
  describe "replace_metadata warnings for event trigger name" do
    it "should fail for creating trigger with invalid name" \testEnvironment -> do
      Postgres.createTable testEnvironment table
      resp <- postMetadataWithStatus 400 testEnvironment (setupMetadataWithTableAndEventTrigger testEnvironment "weird$name" False)
      Postgres.dropTable testEnvironment table
      resp
        `shouldBe` [yaml|
          code: not-supported
          error: Starting in v2.50 only alphanumeric and underscore and hyphens allowed for name
          path: $.args
        |]

    it "should fail for creating trigger with invalid name even when allowing warnings" \testEnvironment -> do
      Postgres.createTable testEnvironment table
      resp <- postMetadataWithStatus 400 testEnvironment (setupMetadataWithTableAndEventTrigger testEnvironment "weird$name" True)
      Postgres.dropTable testEnvironment table
      resp
        `shouldBe` [yaml|
          code: not-supported
          error: Starting in v2.50 only alphanumeric and underscore and hyphens allowed for name
          path: $.args
        |]

setupMetadataWithTableAndEventTrigger :: TestEnvironment -> Text -> Bool -> Value
setupMetadataWithTableAndEventTrigger testEnvironment eventTriggerName allowWarnings =
  [yaml|
    type: replace_metadata
    args:
      allow_inconsistent_metadata: true
      allow_warnings: *allowWarnings
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
                  - name: *eventTriggerName
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
