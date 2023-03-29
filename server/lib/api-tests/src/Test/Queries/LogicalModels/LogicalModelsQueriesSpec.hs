{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Queries.LogicalModels.LogicalModelsQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (options), getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] $
    Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Cockroach.setupTablesAction schema testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Citus.setupTablesAction schema testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Sqlserver.setupTablesAction schema testEnvironment
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

-- we add and track a table here as it's the only way we can currently define a
-- return type
schema :: [Schema.Table]
schema =
  [ (table "stuff")
      { tableColumns =
          [ Schema.column "thing" Schema.TInt,
            Schema.column "date" Schema.TUTCTime
          ]
      }
  ]

tests :: SpecWith TestEnvironment
tests = do
  let query :: Text
      query = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")"

      helloWorldLogicalModel :: Schema.LogicalModel
      helloWorldLogicalModel =
        (Schema.logicalModel "hello_world_function" query)
          { Schema.logicalModelColumns =
              [ Schema.logicalModelColumn "one" Schema.TStr,
                Schema.logicalModelColumn "two" Schema.TStr
              ]
          }

  describe "Testing Logical Models" $ do
    it "Explain works" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel sourceName helloWorldLogicalModel testEnvironment

      let explain :: Value
          explain =
            [graphql|
              query {
                hello_world_function (where: { two: { _eq: "world" } }){
                  one
                  two
                }
              }
           |]

          expected =
            [interpolateYaml|
              [{
                  "field": "hello_world_function"
                }]
              |]

      actual <- GraphqlEngine.postExplain testEnvironment explain

      actual `shouldAtLeastBe` expected

    it "Descriptions and nullability appear in the schema" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          nullableQuery = "SELECT thing / 2 AS divided, null as something_nullable FROM stuff"

          descriptionsAndNullableLogicalModel :: Schema.LogicalModel
          descriptionsAndNullableLogicalModel =
            (Schema.logicalModel "divided_stuff" nullableQuery)
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelColumn "divided" Schema.TInt)
                      { Schema.logicalModelColumnDescription = Just "A divided thing"
                      },
                    (Schema.logicalModelColumn "something_nullable" Schema.TInt)
                      { Schema.logicalModelColumnDescription = Just "Something nullable",
                        Schema.logicalModelColumnNullable = True
                      }
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "unused" Schema.TInt
                  ],
                Schema.logicalModelReturnTypeDescription = Just "Return type description"
              }

      Schema.trackLogicalModel sourceName descriptionsAndNullableLogicalModel testEnvironment

      let queryTypesIntrospection :: Value
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

      actual <- GraphqlEngine.postGraphql testEnvironment queryTypesIntrospection

      actual `shouldBeYaml` expected

    it "Runs the absolute simplest query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
                    - one: "welcome"
                      two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Runs simple query with a basic where clause" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function (where: { two: { _eq: "world" } }){
                  one
                  two
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Runs a simple query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel sourceName helloWorldLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function(where: {one: {_eq: "hello"}}) {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Runs a simple query that takes one dummy parameter and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          helloWorldLogicalModelWithDummyArgument :: Schema.LogicalModel
          helloWorldLogicalModelWithDummyArgument =
            (Schema.logicalModel "hello_world_function_with_dummy" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "dummy" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldLogicalModelWithDummyArgument testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function_with_dummy:
                    - two: "world"
                    - two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function_with_dummy(args: {dummy: "ignored"}, order_by: {one: asc}) {
                  two
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Runs a simple query that takes no parameters but ends with a comment" $ \testEnvironment -> do
      let spicyQuery :: Text
          spicyQuery = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\") -- my query"

      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          helloCommentLogicalModel :: Schema.LogicalModel
          helloCommentLogicalModel =
            (Schema.logicalModel "hello_comment_function" spicyQuery)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloCommentLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_comment_function:
                    - one: "hello"
                      two: "world"
                    - one: "welcome"
                      two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_comment_function {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Uses a column permission that we are allowed to access" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermLogicalModel :: Schema.LogicalModel
          helloWorldPermLogicalModel =
            (Schema.logicalModel "hello_world_perms" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldPermLogicalModel testEnvironment

      shouldReturnYaml
        (options testEnvironment)
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    root_field_name: hello_world_perms
                    role: "test"
                    permission:
                      columns:
                        - one
                      filter: {}
            |]
        )
        [yaml|
          - message: success
        |]

      let expected =
            [yaml|
                data:
                  hello_world_perms:
                    - one: "hello"
                    - one: "welcome"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [("X-Hasura-Role", "test")]
              [graphql|
              query {
                hello_world_perms {
                  one
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Fails because we access a column we do not have permissions for" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermLogicalModel :: Schema.LogicalModel
          helloWorldPermLogicalModel =
            (Schema.logicalModel "hello_world_perms" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldPermLogicalModel testEnvironment

      shouldReturnYaml
        (options testEnvironment)
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    root_field_name: hello_world_perms
                    role: "test"
                    permission:
                      columns:
                        - two
                      filter: {}
            |]
        )
        [yaml|
          - message: success
        |]

      let expected =
            [yaml|
                errors:
                  - extensions:
                      code: validation-failed
                      path: $.selectionSet.hello_world_perms.selectionSet.one
                    message: "field 'one' not found in type: 'hello_world_perms'"
            |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [("X-Hasura-Role", "test")]
              [graphql|
              query {
                hello_world_perms {
                  one
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Using row permissions filters out some results" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermLogicalModel :: Schema.LogicalModel
          helloWorldPermLogicalModel =
            (Schema.logicalModel "hello_world_perms" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldPermLogicalModel testEnvironment

      shouldReturnYaml
        (options testEnvironment)
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    root_field_name: hello_world_perms
                    role: "test"
                    permission:
                      columns: "*"
                      filter:
                        one:
                          _eq: "welcome"
            |]
        )
        [yaml|
          - message: success
        |]

      let expected =
            [yaml|
                data:
                  hello_world_perms:
                    - one: "welcome"
                      two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [("X-Hasura-Role", "test")]
              [graphql|
              query {
                hello_world_perms {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml (options testEnvironment) actual expected
