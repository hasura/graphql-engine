{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Metadata API tests for Data Connector Backend
module Test.DataConnector.MockAgent.MetadataApiSpec where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Aeson.Lens (_Array)
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as Vector
import Harness.Backend.DataConnector.Mock (MockRequestResults (..), mockAgentMetadataTest)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Backend.DataConnector.Mock.Server (defaultMockRequestConfig)
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml (interpolateYaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Hasura.Prelude
import Test.HUnit (assertFailure)
import Test.Hspec (SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

sourceMetadata :: J.Value
sourceMetadata =
  let source = BackendType.backendSourceName Mock.backendTypeMetadata
      backendType = BackendType.backendTypeString Mock.backendTypeMetadata
   in [yaml|
        name : *source
        kind: *backendType
        tables: []
        configuration:
          value: {}
          template: |
            {
              "DEBUG": { "test": "data" }
            }
      |]

--------------------------------------------------------------------------------

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = do
  describe "MetadataAPI Mock Tests" $ do
    mockAgentMetadataTest "Should perform a template transform when calling get_source_tables" $ \testEnvironment performMetadataRequest -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          backendType = BackendType.backendTypeString backendTypeMetadata

      sourceString <-
        BackendType.backendSourceName
          <$> getBackendTypeConfig testEnvironment
          `onNothing` assertFailure "Backend source name not found in test environment"

      let request =
            [interpolateYaml|
              type: #{backendType}_get_source_tables
              args:
                source: #{sourceString}
            |]

      let expectedStatusCode = 200
      MockRequestResults {..} <- performMetadataRequest defaultMockRequestConfig expectedStatusCode request

      J.toJSON _mrrRecordedRequestConfig
        `shouldBe` [yaml|
            DEBUG:
              test: data
          |]

      -- The order of the results can be arbitrary, so we sort to produce a consistent view
      let sortedResponse = _mrrResponse & _Array %~ (Vector.fromList . sort . Vector.toList)
      sortedResponse
        `shouldBe` [yaml|
          - - Album
          - - Artist
          - - Customer
          - - Employee
          - - Genre
          - - Invoice
          - - InvoiceLine
          - - MediaType
          - - MyCustomScalarsTable
          - - Track
        |]
