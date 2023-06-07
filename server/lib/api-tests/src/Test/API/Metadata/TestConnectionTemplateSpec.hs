{-# LANGUAGE QuasiQuotes #-}

-- | Test the test_connection_template metadata API
module Test.API.Metadata.TestConnectionTemplateSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [setupMetadata testEnv]
            }
        ]
    )
    tests

-- ** Setup

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnv = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
      sourceName = Fixture.backendSourceName backendTypeMetadata
      sourceConfiguration = Postgres.defaultSourceConfiguration testEnv

      setup :: IO ()
      setup =
        GraphqlEngine.postMetadata_
          testEnv
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
      teardown =
        GraphqlEngine.postMetadata_
          testEnv
          [yaml|
            type: replace_metadata
            args:
              metadata:
                version: 3
                sources: []
          |]

  Fixture.SetupAction setup \_ -> teardown

-- * Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "test_connection_template negative tests" do
    it "should fail for other backends" $ \testEnv -> do
      shouldReturnYaml
        testEnv
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: mssql_test_connection_template
              args:
            |]
        )
        [yaml|
          code: parse-failed
          error: unknown metadata command "test_connection_template" for backend mssql
          path: "$"
        |]
    it "should throws error for CE" $ \testEnv -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
          sourceName = Fixture.backendSourceName backendTypeMetadata
      shouldReturnYaml
        testEnv
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: pg_test_connection_template
              args:
                source_name: *sourceName
                request_context:
                  session:
                    x-hasura-role: user
            |]
        )
        [yaml|
          code: not-supported
          error: Connection templating feature is enterprise edition only
          path: "$.args"
        |]
