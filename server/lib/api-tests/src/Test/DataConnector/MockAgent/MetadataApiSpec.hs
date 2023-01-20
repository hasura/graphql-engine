{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Metadata API tests for Data Connector Backend
module Test.DataConnector.MockAgent.MetadataApiSpec where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.IORef qualified as IORef
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as Vector
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYamlF)
import Hasura.Backends.DataConnector.API.V0.ConfigSchema (Config (..))
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, pendingWith, shouldBe)

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

sourceMetadata :: Aeson.Value
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

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests opts = do
  describe "MetadataAPI Mock Tests" $ do
    it "Should peform a template transform when calling _get_source_tables" $ \(testEnvironment, Mock.MockAgentEnvironment {maeRecordedRequestConfig}) -> do
      let sortYamlArray :: Aeson.Value -> IO Aeson.Value
          sortYamlArray (Aeson.Array a) = pure $ Aeson.Array (Vector.fromList (sort (Vector.toList a)))
          sortYamlArray _ = fail "Should return Array"

      case BackendType.backendSourceName <$> getBackendTypeConfig testEnvironment of
        Nothing -> pendingWith "Backend not found for testEnvironment"
        Just sourceString -> do
          queryConfig <- IORef.readIORef maeRecordedRequestConfig
          IORef.writeIORef maeRecordedRequestConfig Nothing

          queryConfig `shouldBe` Just (Config $ KM.fromList [("DEBUG", Aeson.Object (KM.fromList [("test", Aeson.String "data")]))])

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
