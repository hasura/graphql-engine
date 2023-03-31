{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the newly separated Custom Return Types feature
module Test.API.Metadata.CustomTypeSpec (spec) where

import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml
import Harness.Schema qualified as Schema
import Harness.Services.GraphqlEngine (emptyHgeConfig, withHge)
import Harness.Services.Metadata (export_metadata, replace_metadata)
import Harness.Services.PostgresSource (withPostgresSource)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig, scalarTypeToText)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec = do
  let fixtures =
        NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Postgres.setupTablesAction schema testEnv
                  ]
              }
          ]

  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] do
    -- need to run isolated
    traverse_
      (Fixture.runClean fixtures)
      [testImplementation]

-- ** Setup and teardown

schema :: [Schema.Table]
schema = []

testImplementation :: SpecWith TestEnvironment
testImplementation = do
  let myCustomType :: Schema.CustomType
      myCustomType =
        (Schema.customType "nice")
          { Schema.customTypeDescription = Just "hello",
            Schema.customTypeColumns =
              [ (Schema.logicalModelColumn "divided" Schema.TInt)
                  { Schema.logicalModelColumnDescription = Just "a divided thing"
                  }
              ]
          }

  describe "When logical models are disabled" do
    let customTypesMetadata =
          [yaml|
                fields:
                  - name: divided
                    nullable: false
                    type: int
                name: divided_stuff
            |]

    let metadataWithCustomReturnType :: Value -> Value
        metadataWithCustomReturnType currentMetadata =
          currentMetadata
            & key "sources"
              . nth 0
              . atKey "custom_return_types"
              .~ Just [yaml| - *customTypesMetadata |]

    withHge emptyHgeConfig $ do
      withPostgresSource "default" $ do
        it "`replace_metadata` preserves custom return types" $ \env -> do
          currentMetadata <- export_metadata env
          _ <- replace_metadata env (metadataWithCustomReturnType currentMetadata)
          actual <- export_metadata env
          actual `shouldBeYaml` (metadataWithCustomReturnType currentMetadata)

        it "`replace_metadata` reports inconsistent objects" $ \env -> do
          currentMetadata <- export_metadata env
          actual <- replace_metadata env (metadataWithCustomReturnType currentMetadata)

          actual
            `shouldBeYaml` [yaml|
              inconsistent_objects:
                - definition: *customTypesMetadata
                  name: custom_type divided_stuff in source default
                  reason: 'Inconsistent object: The Logical Models feature is disabled'
                  type: custom_type 
              is_consistent: false
            |]

  describe "Implementation" $ do
    it "Adds a simple custom type and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackCustomType sourceName myCustomType testEnvironment

    it "Checks for the custom type" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_custom_return_type"

      Schema.trackCustomType sourceName myCustomType testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: *getRequestType 
              args:
                source: *sourceName
            |]
        )
        [interpolateYaml|
          - name: nice 
            description: hello
            fields:
                - name: divided 
                  type: #{scalarTypeToText testEnvironment Schema.TInt}
                  nullable: false
                  description: "a divided thing"
        |]

    it "Checks the custom type is deleted again" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_custom_return_type"

      Schema.trackCustomType sourceName myCustomType testEnvironment

      Schema.untrackCustomType sourceName myCustomType testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: *getRequestType 
              args:
                source: *sourceName
            |]
        )
        [yaml|
          []
        |]

    it "Can't delete a custom type that is in use" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          logicalModel :: Schema.LogicalModel
          logicalModel =
            (Schema.logicalModel "logical_model" "SELECT 1 as divided" "nice")

      Schema.trackCustomType sourceName myCustomType testEnvironment
      Schema.trackLogicalModel sourceName logicalModel testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus 400 testEnvironment $
            Schema.untrackCustomTypeCommand sourceName backendTypeMetadata myCustomType
        )
        [yaml|
          code: constraint-violation
          error: Custom type "nice" still being used by logical model "logical_model".
          path: $.args
        |]
