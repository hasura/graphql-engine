{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Logical Models feature.
module Test.API.Metadata.LogicalModelsSpec (spec) where

import Data.Aeson qualified as A
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

-- We currently don't need the table to exist in order to set up a logical model
-- stanza.

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
    -- do not need to run isolated
    traverse_
      (Fixture.run fixtures)
      [testAdminAccess, testValidation, testPermissionFailures]
    -- need to run isolated
    traverse_
      (Fixture.runClean fixtures)
      [testImplementation, testPermissions]

-- ** Setup and teardown

schema :: [Schema.Table]
schema =
  [ (Schema.table "stuff")
      { Schema.tableColumns =
          [ Schema.column "thing" Schema.TInt,
            Schema.column "date" Schema.TUTCTime
          ]
      }
  ]

testAdminAccess :: Fixture.Options -> SpecWith TestEnvironment
testAdminAccess opts = do
  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Admin access" do
    it "Fails to track a Logical Model without admin access" $
      \testEnv -> do
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnv
              [ ("X-Hasura-Role", "not-admin")
              ]
              [yaml|
                type: pg_track_logical_model
                args:
                  type: query
                  source: postgres
                  root_field_name: divided_stuff
                  code: *query
                  arguments:
                    denominator:
                      type: integer
                    target_date:
                      type: date
                  returns:
                    columns:
                      divided:
                        type: integer
                        description: "a divided thing"
              |]
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

    it "Fails to untrack a Logical Model without admin access" $
      \testEnv -> do
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnv
              [ ("X-Hasura-Role", "not-admin")
              ]
              [yaml|
                type: pg_untrack_logical_model
                args:
                  root_field_name: divided_stuff
                  source: postgres
              |]
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

    it "Fails to list a Logical Model without admin access" $
      \testEnv -> do
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnv
              [ ("X-Hasura-Role", "not-admin")
              ]
              [yaml|
                type: pg_get_logical_model
                args:
                  source: postgres
              |]
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

-------------------------
-- Test implementation --
-------------------------

testImplementation :: Fixture.Options -> SpecWith TestEnvironment
testImplementation opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Implementation" $ do
    it "Adds a simple logical model of a function with no arguments and returns a 200" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                code: *simpleQuery
                arguments:
                  unused:
                    type: integer
                returns:
                  columns:
                    divided:
                      type: integer
                      description: "a divided thing"
            |]
        )
        [yaml|
          message: success
        |]

    it "Adding a logical model of a function with broken SQL returns a 400" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                code: "SELECT * FROM dogs WHERE name = {{name"
                arguments:
                  denominator:
                    type: integer
                  target_date:
                    type: date
                returns:
                  columns:
                    divided:
                      type: integer
                      description: "a divided thing"
            |]
        )
        [yaml|
          code: parse-failed
          error: "Found '{{' without a matching closing '}}'"
          path: "$.args"
        |]

    it "Checks for the logical model of a function" $ \testEnv -> do
      let rootfield :: String
          rootfield = "divided_stuff2"
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: postgres
                root_field_name: *rootfield
                code: *query
                arguments:
                  denominator:
                    type: integer
                  target_date:
                    type: date
                returns:
                  columns:
                    divided:
                      type: integer
                      description: "a divided thing"
            |]
        )
        [yaml|
            message: success
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_get_logical_model
              args:
                source: postgres
            |]
        )
        [yaml|
          - root_field_name: *rootfield
            code: *query
            arguments:
              denominator:
                type: integer
                nullable: false
              target_date:
                type: date
                nullable: false
            returns:
              columns:
                    divided:
                      type: integer
                      nullable: false
                      description: "a divided thing"
        |]

    it "Drops a logical model of a function and returns a 200" $ \testEnv -> do
      _ <-
        GraphqlEngine.postMetadata
          testEnv
          [yaml|
            type: pg_track_logical_model
            args:
              type: query
              source: postgres
              root_field_name: divided_stuff
              code: *query
              arguments:
                denominator:
                  type: integer
                target_date:
                  type: date
              returns:
                columns:
                  divided:
                    type: integer
                    description: "a divided thing"
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_untrack_logical_model
              args:
                source: postgres
                root_field_name: divided_stuff
            |]
        )
        [yaml|
          message: success
        |]

    it "Checks the logical model of a function can be deleted" $ \testEnv -> do
      _ <-
        GraphqlEngine.postMetadata
          testEnv
          [yaml|
            type: pg_track_logical_model
            args:
              type: query
              source: postgres
              root_field_name: divided_stuff
              code: *query
              arguments:
                denominator:
                  type: integer
                target_date:
                  type: date
              returns:
                columns:
                  divided:
                    type: integer
                    description: "a divided thing"
          |]

      _ <-
        GraphqlEngine.postMetadata
          testEnv
          [yaml|
            type: pg_untrack_logical_model
            args:
              root_field_name: divided_stuff
              source: postgres
          |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_get_logical_model
              args:
                source: postgres
            |]
        )
        [yaml|
          []
        |]

    it "Descriptions and nullability appear in the schema" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: postgres
                root_field_name: divided_stuff
                code: |
                  SELECT thing / 2 AS divided, null as something_nullable FROM stuff
                arguments:
                  unused:
                    type: integer
                returns:
                  description: "Return type description"
                  columns:
                    divided:
                      type: integer
                      description: "A divided thing"
                    something_nullable:
                      type: integer
                      description: "Something nullable"
                      nullable: true
            |]
        )
        [yaml|
          message: success
        |]

      let queryTypesIntrospection :: A.Value
          queryTypesIntrospection =
            [graphql|
                query {
                  __type(name: "divided_stuff") {
                    name
                    description
                    fields {
                      name
                      description
                      type {
                        name
                        kind
                        ofType {
                          name
                        }
                      }
                    }
                  }
                }
              |]

          expected =
            [interpolateYaml|
                {
                  "data": {
                    "__type": {
                      "description": "Return type description",
                      "fields": [
                      {
                        "description": "A divided thing",
                        "name": "divided",
                        "type": {
                          "kind": "NON_NULL",
                          "name": null,
                          "ofType": {
                            "name": "Int"
                          }
                        }
                      },
                      {
                        "description": "Something nullable",
                        "name": "something_nullable",
                        "type": {
                          "kind": "SCALAR",
                          "name": "Int",
                          "ofType": null
                        }
                      }
                      ],
                      "name": "divided_stuff"
                    }
                  }
                }
              |]

      actual <- GraphqlEngine.postGraphql testEnv queryTypesIntrospection

      actual `shouldBeYaml` expected

---------------------
-- Test validation --
---------------------

testValidation :: Fixture.Options -> SpecWith TestEnvironment
testValidation opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  describe "Validation fails on untrack a logical model" do
    it "when a logical model does not exist" $
      \testEnv -> do
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              [yaml|
              type: pg_untrack_logical_model
              args:
                root_field_name: some_logical_model
                source: postgres
            |]
          )
          [yaml|
          code: not-found
          error: "Logical model \"some_logical_model\" not found in source \"postgres\"."
          path: "$.args"
        |]

  describe "Validation fails on track a logical model" do
    it "when the query has a syntax error" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "query bad"

        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                  path: "$.args"
              |]
        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
                type: pg_track_logical_model
                args:
                  type: query
                  source: postgres
                  root_field_name: divided_stuff
                  code: *spicyQuery
                  arguments:
                    denominator:
                      type: integer
                    target_date:
                      type: date
                  returns:
                    columns:
                      divided:
                        type: integer
                        description: "a divided thing"
              |]

        actual `shouldAtLeastBe` expected

    it "when the query refers to non existing table" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "SELECT thing / {{denominator}} AS divided FROM does_not_exist WHERE date = {{target_date}}"

        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                  internal:
                    error:
                      message: "relation \"does_not_exist\" does not exist"
                      status_code: "42P01"
              |]

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
                type: pg_track_logical_model
                args:
                  type: query
                  source: postgres
                  root_field_name: divided_stuff
                  code: *spicyQuery
                  arguments:
                    denominator:
                      type: integer
                    target_date:
                      type: date
                  returns:
                    columns:
                      divided:
                        type: integer
                        description: "a divided thing"
              |]

        actual `shouldAtLeastBe` expected

    it "when the logical model has the same name as an already tracked table" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "select * from stuff"
            backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
            source = BackendType.backendSourceName backendTypeMetadata
            schemaName = Schema.getSchemaName testEnv

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              500
              testEnv
              [interpolateYaml|
                type: pg_track_logical_model
                args:
                  type: query
                  source: #{source}
                  root_field_name: #{schemaName}_stuff
                  code: #{spicyQuery}
                  arguments:
                    denominator:
                      type: integer
                    target_date:
                      type: date
                  returns:
                    columns:
                      thing:
                        type: integer
                      date:
                        type: date
              |]
          )
          [yaml|
              code: unexpected
              error: Encountered conflicting definitions in the selection set for 'subscription_root' for field 'hasura_stuff' defined in [table hasura.stuff in source postgres, logical_model hasura_stuff in source postgres]. Fields must not be defined more than once across all sources.
              path: $.args
          |]

    it "when the logical model has the same name as an already tracked logical model" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "select * from stuff"
            backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
            source = BackendType.backendSourceName backendTypeMetadata
            schemaName = Schema.getSchemaName testEnv

            trackLogimoReq =
              [interpolateYaml|
                type: pg_track_logical_model
                args:
                  type: query
                  source: #{source}
                  root_field_name: #{schemaName}_stuff_exist
                  code: #{spicyQuery}
                  arguments:
                    denominator:
                      type: integer
                    target_date:
                      type: date
                  returns:
                    columns:
                      thing:
                        type: integer
                      date:
                        type: date
              |]

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadata
              testEnv
              trackLogimoReq
          )
          [yaml|
              message: success
          |]

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              trackLogimoReq
          )
          [yaml|
              code: already-tracked
              error: Logical model 'hasura_stuff_exist' is already tracked.
              path: $.args
          |]

    it "where arguments do not typecheck" $
      \testEnv -> do
        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                |]
        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
                  type: pg_track_logical_model
                  args:
                    type: query
                    source: postgres
                    root_field_name: divided_failing
                    code: |
                      SELECT 10 / {{denominator}} AS divided
                    arguments:
                      denominator:
                        type: varchar
                    returns:
                      columns:
                        divided:
                          type: integer
            |]

        actual `shouldAtLeastBe` expected

    it "that uses undeclared arguments" $
      \testEnv -> do
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              [yaml|
                  type: pg_track_logical_model
                  args:
                    type: query
                    source: postgres
                    root_field_name: divided_failing
                    code: |
                      SELECT 10 / {{denominator}} AS divided
                    returns:
                      columns:
                        divided:
                          type: integer
            |]
          )
          [yaml|
             code: validation-failed
             error: 'Undeclared arguments: "denominator"'
             path: $.args
          |]

  describe "Validation succeeds" do
    it "when tracking then untracking then re-tracking a logical model" $
      \testEnv -> do
        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadata
              testEnv
              [yaml|
                type: bulk
                args:
                  - type: pg_track_logical_model
                    args:
                      type: query
                      source: postgres
                      root_field_name: divided_stuff2
                      code: *simpleQuery
                      arguments:
                        denominator:
                          type: integer
                        target_date:
                          type: date
                      returns:
                        columns:
                          divided:
                            type: integer
                  - type: pg_untrack_logical_model
                    args:
                      root_field_name: divided_stuff2
                      source: postgres
                  - type: pg_track_logical_model
                    args:
                      type: query
                      source: postgres
                      root_field_name: divided_stuff2
                      code: *simpleQuery
                      arguments:
                        denominator:
                          type: integer
                        target_date:
                          type: date
                      returns:
                        columns:
                          divided:
                            type: integer
            |]
          )
          [yaml|
            - message: success
            - message: success
            - message: success
          |]

----------------------
-- Test permissions --
----------------------

testPermissions :: Fixture.Options -> SpecWith TestEnvironment
testPermissions opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  describe "Permissions" do
    it "Adds a simple logical model function with no arguments a select permission and returns a 200" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: bulk
              args:
                - type: pg_track_logical_model
                  args:
                    type: query
                    source: postgres
                    root_field_name: divided_stuff
                    code: *simpleQuery
                    arguments:
                      unused:
                        type: integer
                    returns:
                      columns:
                        divided:
                          type: integer
                          description: "a divided thing"
                - type: pg_create_logical_model_select_permission
                  args:
                    source: postgres
                    root_field_name: divided_stuff
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [yaml|
          - message: success
          - message: success
        |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_get_logical_model
              args:
                source: postgres
            |]
        )
        [yaml|
          - root_field_name: divided_stuff
            code: *simpleQuery
            arguments:
              unused:
                type: integer
                nullable: false
            select_permissions:
              - role: "test"
                permission:
                  columns:
                    - divided
                  filter: {}
            returns:
              columns:
                divided:
                  description: a divided thing
                  nullable: false
                  type: integer
        |]

    it "Adds a logical model, removes it, and returns 200" $ \testEnv -> do
      let rootfield :: String
          rootfield = "divided_stuff1231"
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: bulk
              args:
                - type: pg_track_logical_model
                  args:
                    type: query
                    source: postgres
                    root_field_name: *rootfield
                    code: *simpleQuery
                    arguments:
                      unused:
                        type: integer
                    returns:
                      columns:
                        divided:
                          type: integer
                          description: "a divided thing"
                - type: pg_create_logical_model_select_permission
                  args:
                    source: postgres
                    root_field_name: *rootfield
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
                - type: pg_drop_logical_model_select_permission
                  args:
                    source: postgres
                    root_field_name: *rootfield
                    role: "test"
            |]
        )
        [yaml|
          - message: success
          - message: success
          - message: success
        |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnv
            [yaml|
              type: pg_get_logical_model
              args:
                source: postgres
            |]
        )
        [yaml|
          - root_field_name: *rootfield
            code: *simpleQuery
            arguments:
              unused:
                type: integer
                nullable: false
            returns:
              columns:
                divided:
                  description: a divided thing
                  nullable: false
                  type: integer
        |]

testPermissionFailures :: Fixture.Options -> SpecWith TestEnvironment
testPermissionFailures opts = do
  describe "Permission failures" do
    it "Fails to adds a select permission to a nonexisting source" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: bulk
              args:
                - type: pg_create_logical_model_select_permission
                  args:
                    source: made_up_source
                    root_field_name: made_up_logical_model
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

    it "Fails to adds a select permission to a nonexisting logical model" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: bulk
              args:
                - type: pg_create_logical_model_select_permission
                  args:
                    source: postgres
                    root_field_name: made_up_logical_model
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [yaml|
          code: "not-found"
          error: "Logical model \"made_up_logical_model\" not found in source \"postgres\"."
          path: "$.args[0].args"
        |]

    it "Fails to drop a select permission on a nonexisting source" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: pg_drop_logical_model_select_permission
              args:
                source: made_up_source
                root_field_name: made_up_logical_model
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
          path: "$.args"
        |]

    it "Fails to drop a select permission from a nonexisting logical model" $ \testEnv -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnv
            [yaml|
              type: pg_drop_logical_model_select_permission
              args:
                source: postgres
                root_field_name: made_up_logical_model
                role: "test"
            |]
        )
        [yaml|
          code: "not-found"
          error: "Logical model \"made_up_logical_model\" not found in source \"postgres\"."
          path: "$.args"
        |]
