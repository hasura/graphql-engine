{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Metadata API tests for Data Connector Backend
module Test.DataConnector.MockAgent.MetadataApiSpec where

--------------------------------------------------------------------------------

import Control.Lens qualified as Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens
import Data.IORef qualified as IORef
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as Vector
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (defaultBackendCapabilities, defaultBackendServerUrl)
import Harness.Test.Fixture (defaultBackendDisplayNameString, defaultBackendTypeString, defaultSource)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.TestEnvironment qualified as TE
import Harness.Yaml (shouldReturnYaml, shouldReturnYamlF)
import Hasura.Backends.DataConnector.API.V0.ConfigSchema (Config (..))
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, pendingWith, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.DataConnectorMock)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource Fixture.DataConnectorMock
      backendType = defaultBackendTypeString Fixture.DataConnectorMock
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
    it "Should peform a template transform when calling _get_source_tables" $ \(testEnvironment, Mock.MockAgentEnvironment {maeQueryConfig}) -> do
      let sortYamlArray :: Aeson.Value -> IO Aeson.Value
          sortYamlArray (Aeson.Array a) = pure $ Aeson.Array (Vector.fromList (sort (Vector.toList a)))
          sortYamlArray _ = fail "Should return Array"

      case defaultSource <$> TE.backendType testEnvironment of
        Nothing -> pendingWith "Backend not found for testEnvironment"
        Just sourceString -> do
          queryConfig <- IORef.readIORef maeQueryConfig
          IORef.writeIORef maeQueryConfig Nothing

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
