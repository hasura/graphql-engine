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
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

-- We currently don't need the table to exist in order to set up a logical model
-- stanza.

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] $
    Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Postgres.setupTablesAction schema testEnv
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

schema :: [Schema.Table]
schema =
  [ (Schema.table "already_tracked_return_type")
      { Schema.tableColumns =
          [ Schema.column "divided" Schema.TInt
          ]
      },
    (Schema.table "stuff")
      { Schema.tableColumns =
          [ Schema.column "thing" Schema.TInt,
            Schema.column "date" Schema.TUTCTime
          ]
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

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
                  denominator: int
                  target_date: date
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
  it
    "Fails to untrack a Logical Model without admin access"
    $ \testEnv -> do
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
  it
    "Fails to list a Logical Model without admin access"
    $ \testEnv -> do
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
                  unused: int
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
                  denominator: int
                  target_date: date
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
                code: *query
                arguments:
                  denominator: int
                  target_date: date
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
          - root_field_name: divided_stuff
            code: *query
            arguments:
              denominator: int
              target_date: date
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
                denominator: int
                target_date: date
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
                denominator: int
                target_date: date
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
                  unused: int
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
          error: "Logical model 'some_logical_model' not found in source 'postgres'."
          path: "$.args"
        |]

  describe "Validation fails on track a logical model when query" do
    it "has a syntax error" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "query bad"
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
                  code: *spicyQuery
                  arguments:
                    denominator: int
                    target_date: date
                  returns:
                    columns:
                      divided:
                        type: integer
                        description: "a divided thing"
              |]
          )
          [yaml|
              code: validation-failed
              error: Failed to validate query
              internal:
                arguments: []
                error:
                  description: null
                  exec_status: "FatalError"
                  hint: null
                  message: "syntax error at or near \"query\""
                  status_code: "42601"
                prepared: false
                statement: "PREPARE _logimo_vali_divided_stuff AS query bad"
              path: "$.args"
          |]

    it "refers to non existing table" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "SELECT thing / {{denominator}} AS divided FROM does_not_exist WHERE date = {{target_date}}"
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
                  code: *spicyQuery
                  arguments:
                    denominator: int
                    target_date: date
                  returns:
                    columns:
                      divided:
                        type: integer
                        description: "a divided thing"
              |]
          )
          [yaml|
              code: validation-failed
              error: Failed to validate query
              internal:
                arguments: []
                error:
                  description: null
                  exec_status: "FatalError"
                  hint: null
                  message: "relation \"does_not_exist\" does not exist"
                  status_code: "42P01"
                prepared: false
                statement: "PREPARE _logimo_vali_divided_stuff AS SELECT thing / $1 AS divided FROM does_not_exist WHERE date = $2"
              path: "$.args"
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
                        denominator: int
                        target_date: date
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
                        denominator: int
                        target_date: date
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
