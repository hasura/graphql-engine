{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the newly separated Logical Models feature
module Test.API.Metadata.BulkKeepGoingSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runClean
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

-- ** Setup and teardown

schema :: [Schema.Table]
schema = []

tests :: SpecWith TestEnvironment
tests = do
  it "returns successes when the /last/ query fails" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

    GraphqlEngine.postV2Query_
      testEnvironment
      [interpolateYaml|
        type: run_sql
        args:
          source: #{sourceName}
          sql: CREATE TABLE test_table (id int);
          cascade: false
          read_only: false
      |]

    actual <-
      GraphqlEngine.postMetadata
        testEnvironment
        [interpolateYaml|
          type: bulk_keep_going
          args:
            - type: pg_track_table
              args:
                source: #{sourceName}
                table:
                  name: test_table
            - type: pg_track_table
              args:
                source: #{sourceName}
                table:
                  name: unknown_table
        |]

    actual
      `shouldAtLeastBe` [interpolateYaml|
        - message: success
        - code: invalid-configuration
      |]

  it "returns successes when the /first/ query fails" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        sourceName = BackendType.backendSourceName backendTypeMetadata

    GraphqlEngine.postV2Query_
      testEnvironment
      [interpolateYaml|
        type: run_sql
        args:
          source: #{sourceName}
          sql: CREATE TABLE test_table (id int);
          cascade: false
          read_only: false
      |]

    actual <-
      GraphqlEngine.postMetadata
        testEnvironment
        [interpolateYaml|
          type: bulk_keep_going
          args:
            - type: pg_track_table
              args:
                source: #{sourceName}
                table:
                  name: unknown_table
            - type: pg_track_table
              args:
                source: #{sourceName}
                table:
                  name: test_table
        |]

    actual
      `shouldAtLeastBe` [interpolateYaml|
        - code: invalid-configuration
        - message: success
      |]
