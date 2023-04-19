{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Tests of the newly separated Logical Models feature
module Test.API.Metadata.LogicalModelSpec (spec) where

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

featureFlagForNativeQueries :: String
featureFlagForNativeQueries = "HASURA_FF_NATIVE_QUERY_INTERFACE"

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

  Fixture.hgeWithEnv [(featureFlagForNativeQueries, "True")] do
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
  let myLogicalModel :: Schema.LogicalModel
      myLogicalModel =
        (Schema.logicalModel "nice")
          { Schema.logicalModelDescription = Just "hello",
            Schema.logicalModelColumns =
              [ (Schema.logicalModelScalar "divided" Schema.TInt)
                  { Schema.logicalModelColumnDescription = Just "a divided thing"
                  }
              ]
          }

  describe "When native queries are disabled" do
    let logicalModelsMetadata =
          [yaml|
                fields:
                  - name: divided
                    nullable: false
                    type: int
                name: divided_stuff
            |]

    let metadataWithLogicalModel :: Value -> Value
        metadataWithLogicalModel currentMetadata =
          currentMetadata
            & key "sources"
              . nth 0
              . atKey "logical_models"
              .~ Just [yaml| - *logicalModelsMetadata |]

    withHge emptyHgeConfig $ do
      withPostgresSource "default" $ do
        it "`replace_metadata` preserves logical models" $ \env -> do
          currentMetadata <- export_metadata env
          _ <- replace_metadata env (metadataWithLogicalModel currentMetadata)
          actual <- export_metadata env
          actual `shouldBeYaml` (metadataWithLogicalModel currentMetadata)

        it "`replace_metadata` reports inconsistent objects" $ \env -> do
          currentMetadata <- export_metadata env
          actual <- replace_metadata env (metadataWithLogicalModel currentMetadata)

          actual
            `shouldBeYaml` [yaml|
              inconsistent_objects:
                - definition: *logicalModelsMetadata
                  name: custom_type divided_stuff in source default
                  reason: 'Inconsistent object: The Logical Model feature is disabled'
                  type: custom_type
              is_consistent: false
            |]

  describe "Implementation" $ do
    it "Adds a simple logical model and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel sourceName myLogicalModel testEnvironment

    it "Adds a logical model with a nested logical model and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          nestedLogicalModel :: Schema.LogicalModel
          nestedLogicalModel =
            (Schema.logicalModel "nested")
              { Schema.logicalModelDescription = Just "hello",
                Schema.logicalModelColumns =
                  [ Schema.logicalModelScalar "name" Schema.TStr,
                    Schema.logicalModelReference "nices" "nice"
                  ]
              }

      Schema.trackLogicalModel sourceName myLogicalModel testEnvironment
      Schema.trackLogicalModel sourceName nestedLogicalModel testEnvironment

    it "Checks for the logical model" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_logical_model"

      Schema.trackLogicalModel sourceName myLogicalModel testEnvironment

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

    it "Checks the logical model is deleted again" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_logical_model"

      Schema.trackLogicalModel sourceName myLogicalModel testEnvironment

      Schema.untrackLogicalModel sourceName myLogicalModel testEnvironment

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

    it "Can't delete a logical model that is in use" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          nativeQuery :: Schema.NativeQuery
          nativeQuery =
            (Schema.nativeQuery "native_query" "SELECT 1 as divided" "nice")

      Schema.trackLogicalModel sourceName myLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName nativeQuery testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus 400 testEnvironment $
            Schema.untrackLogicalModelCommand sourceName backendTypeMetadata myLogicalModel
        )
        [yaml|
          code: constraint-violation
          error: Custom type "nice" still being used by native query "native_query".
          path: $.args
        |]

----------------------
-- Test permissions --
----------------------

testPermissions :: SpecWith TestEnvironment
testPermissions = do
  let logicalModel :: Schema.LogicalModel
      logicalModel =
        (Schema.logicalModel "divided_stuff")
          { Schema.logicalModelColumns =
              [ (Schema.logicalModelScalar "divided" Schema.TInt)
                  { Schema.logicalModelColumnDescription = Just "a divided thing"
                  }
              ]
          }

  describe "Permissions" do
    it "Adds a logical model with a select permission and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata

      Schema.trackLogicalModel sourceName logicalModel testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [interpolateYaml|
              type: bulk
              args:
                - type: #{backendType}_create_logical_model_select_permission
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
              type: #{backendType}_get_logical_model
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

    it "Adds a native query, removes it, and returns 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata

      Schema.trackLogicalModel sourceName logicalModel testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [interpolateYaml|
              type: bulk
              args:
                - type: #{backendType}_create_logical_model_select_permission
                  args:
                    source: #{sourceName}
                    name: divided_stuff
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
                - type: #{backendType}_drop_logical_model_select_permission
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
              type: #{backendType}_get_logical_model
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
                - type: #{backendType}_create_logical_model_select_permission
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

    it "Fails to adds a select permission to a nonexisting logical model" $ \testEnvironment -> do
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
                - type: #{backendType}_create_logical_model_select_permission
                  args:
                    source: #{sourceName}
                    name: made_up_logical_model
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [interpolateYaml|
          code: "not-found"
          error: Logical model "made_up_logical_model" not found in source "#{sourceName}".
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
              type: #{backendType}_drop_logical_model_select_permission
              args:
                source: made_up_source
                name: made_up_logical_model
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

    it "Fails to drop a select permission from a nonexisting logical model" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [interpolateYaml|
              type: #{backendType}_drop_logical_model_select_permission
              args:
                source: #{sourceName}
                name: made_up_logical_model
                role: "test"
            |]
        )
        [interpolateYaml|
          code: "not-found"
          error: Logical model "made_up_logical_model" not found in source "#{sourceName}".
          path: "$.args"
        |]
