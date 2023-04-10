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
import Harness.Services.Source.Postgres (withPostgresSource)
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
    traverse_
      (Fixture.runClean fixtures)
      [ testImplementation,
        testPermissions,
        testPermissionFailures
      ]

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
                  reason: 'Inconsistent object: The Custom Return Type feature is disabled'
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

----------------------
-- Test permissions --
----------------------

testPermissions :: SpecWith TestEnvironment
testPermissions = do
  let customReturnType :: Schema.CustomType
      customReturnType =
        (Schema.customType "divided_stuff")
          { Schema.customTypeColumns =
              [ (Schema.logicalModelColumn "divided" Schema.TInt)
                  { Schema.logicalModelColumnDescription = Just "a divided thing"
                  }
              ]
          }

  describe "Permissions" do
    it "Adds a custom return type with a select permission and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata

      Schema.trackCustomType sourceName customReturnType testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [interpolateYaml|
              type: bulk
              args:
                - type: #{backendType}_create_custom_return_type_select_permission
                  args:
                    source: #{sourceName}
                    name: divided_stuff
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [yaml|
          - message: success
        |]

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [interpolateYaml|
              type: #{backendType}_get_custom_return_type
              args:
                source: #{sourceName}
            |]
        )
        [interpolateYaml|
          - name: divided_stuff
            description: ''
            fields:
            - description: a divided thing
              name: divided
              nullable: false
              type: integer
            select_permissions:
              - role: "test"
                permission:
                  columns:
                    - divided
                  filter: {}
        |]

    it "Adds a logical model, removes it, and returns 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata

      Schema.trackCustomType sourceName customReturnType testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [interpolateYaml|
              type: bulk
              args:
                - type: #{backendType}_create_custom_return_type_select_permission
                  args:
                    source: #{sourceName}
                    name: divided_stuff
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
                - type: #{backendType}_drop_custom_return_type_select_permission
                  args:
                    source: #{sourceName}
                    name: divided_stuff
                    role: "test"
            |]
        )
        [yaml|
          - message: success
          - message: success
        |]

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [interpolateYaml|
              type: #{backendType}_get_custom_return_type
              args:
                source: #{sourceName}
            |]
        )
        [interpolateYaml|
          - name: divided_stuff
            description: ''
            fields:
            - description: a divided thing
              name: divided
              nullable: false
              type: integer
        |]

testPermissionFailures :: SpecWith TestEnvironment
testPermissionFailures = do
  describe "Permission failures" do
    it "Fails to adds a select permission to a nonexisting source" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          backendType = BackendType.backendTypeString backendTypeMetadata

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [interpolateYaml|
              type: bulk
              args:
                - type: #{backendType}_create_custom_return_type_select_permission
                  args:
                    source: made_up_source
                    name: divided_stuff
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [yaml|
          code: not-found
          error: "Source \"made_up_source\" not found."
          path: "$.args[0].args"
        |]

    it "Fails to adds a select permission to a nonexisting custom return type" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [interpolateYaml|
              type: bulk
              args:
                - type: #{backendType}_create_custom_return_type_select_permission
                  args:
                    source: #{sourceName}
                    name: made_up_custom_return_type
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [interpolateYaml|
          code: "not-found"
          error: Custom return type "made_up_custom_return_type" not found in source "#{sourceName}".
          path: "$.args[0].args"
        |]

    it "Fails to drop a select permission on a nonexisting source" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          backendType = BackendType.backendTypeString backendTypeMetadata

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [interpolateYaml|
              type: #{backendType}_drop_custom_return_type_select_permission
              args:
                source: made_up_source
                name: made_up_custom_return_type
                role: "test"
                permission:
                  columns:
                    - divided
                  filter: {}
            |]
        )
        [interpolateYaml|
          code: not-found
          error: "Source \"made_up_source\" not found."
          path: "$.args"
        |]

    it "Fails to drop a select permission from a nonexisting custom return type" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [interpolateYaml|
              type: #{backendType}_drop_custom_return_type_select_permission
              args:
                source: #{sourceName}
                name: made_up_custom_return_type
                role: "test"
            |]
        )
        [interpolateYaml|
          code: "not-found"
          error: Custom return type "made_up_custom_return_type" not found in source "#{sourceName}".
          path: "$.args"
        |]
