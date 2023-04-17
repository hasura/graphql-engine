{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Access to the SQL
module Test.Queries.NativeQueries.NativeQueriesSpec (spec) where

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
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForNativeQueries :: String
featureFlagForNativeQueries = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForNativeQueries, "True")] $
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

      helloWorldReturnType :: Schema.CustomReturnType
      helloWorldReturnType =
        (Schema.customType "hello_world_return_type")
          { Schema.customTypeColumns =
              [ Schema.customReturnTypeScalar "one" Schema.TStr,
                Schema.customReturnTypeScalar "two" Schema.TStr
              ]
          }

      helloWorldNativeQuery :: Schema.NativeQuery
      helloWorldNativeQuery =
        (Schema.nativeQuery "hello_world_function" query "hello_world_return_type")

  describe "Testing Native Queries" $ do
    it "Explain works" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackCustomReturnType sourceName helloWorldReturnType testEnvironment

      Schema.trackNativeQuery sourceName helloWorldNativeQuery testEnvironment

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

          nullableQuery = "SELECT (thing / 2)::integer AS divided, null::text as something_nullable FROM stuff"

          descriptionsAndNullableReturnType :: Schema.CustomReturnType
          descriptionsAndNullableReturnType =
            (Schema.customType "divided_stuff")
              { Schema.customTypeColumns =
                  [ (Schema.customReturnTypeScalar "divided" Schema.TInt)
                      { Schema.customReturnTypeColumnDescription = Just "A divided thing"
                      },
                    (Schema.customReturnTypeScalar "something_nullable" Schema.TStr)
                      { Schema.customReturnTypeColumnDescription = Just "Something nullable",
                        Schema.customReturnTypeColumnNullable = True
                      }
                  ],
                Schema.customTypeDescription = Just "Return type description"
              }

          descriptionsAndNullableNativeQuery :: Schema.NativeQuery
          descriptionsAndNullableNativeQuery =
            (Schema.nativeQuery "divided_stuff" nullableQuery "divided_stuff")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "unused" Schema.TInt
                  ]
              }

      Schema.trackCustomReturnType sourceName descriptionsAndNullableReturnType testEnvironment

      Schema.trackNativeQuery sourceName descriptionsAndNullableNativeQuery testEnvironment

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
                          "name": "String",
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

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment
      Schema.trackNativeQuery source helloWorldNativeQuery testEnvironment

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

      shouldReturnYaml testEnvironment actual expected

    it "Runs simple query with a basic where clause" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment
      Schema.trackNativeQuery source helloWorldNativeQuery testEnvironment

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

      shouldReturnYaml testEnvironment actual expected

    it "Runs a simple query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackCustomReturnType sourceName helloWorldReturnType testEnvironment
      Schema.trackNativeQuery sourceName helloWorldNativeQuery testEnvironment

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

      shouldReturnYaml testEnvironment actual expected

    it "Runs a simple query that takes one dummy parameter and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          helloWorldNativeQueryWithDummyArgument :: Schema.NativeQuery
          helloWorldNativeQueryWithDummyArgument =
            (Schema.nativeQuery "hello_world_function_with_dummy" query "hello_world_return_type")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "dummy" Schema.TStr
                  ]
              }

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment

      Schema.trackNativeQuery source helloWorldNativeQueryWithDummyArgument testEnvironment

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

      shouldReturnYaml testEnvironment actual expected

    it "Runs a simple query that takes no parameters but ends with a comment" $ \testEnvironment -> do
      let spicyQuery :: Text
          spicyQuery = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\") -- my query"

      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          helloCommentNativeQuery :: Schema.NativeQuery
          helloCommentNativeQuery =
            (Schema.nativeQuery "hello_comment_function" spicyQuery "hello_world_return_type")

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment

      Schema.trackNativeQuery source helloCommentNativeQuery testEnvironment

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

      shouldReturnYaml testEnvironment actual expected

    it "Uses a column permission that we are allowed to access" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_custom_return_type_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_with_permissions" query "hello_world_return_type")

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment

      Schema.trackNativeQuery source helloWorldPermNativeQuery testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    name: hello_world_return_type
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
                  hello_world_with_permissions:
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
                hello_world_with_permissions {
                  one
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Fails because we access a column we do not have permissions for" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_custom_return_type_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_with_permissions" query "hello_world_return_type")

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment

      Schema.trackNativeQuery source helloWorldPermNativeQuery testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    name: hello_world_return_type
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
                      path: $.selectionSet.hello_world_with_permissions.selectionSet.one
                    message: "field 'one' not found in type: 'hello_world_return_type'"
            |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [("X-Hasura-Role", "test")]
              [graphql|
              query {
                hello_world_with_permissions {
                  one
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Using row permissions filters out some results" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_custom_return_type_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_with_permissions" query "hello_world_return_type")

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment

      Schema.trackNativeQuery source helloWorldPermNativeQuery testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    name: hello_world_return_type
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
                  hello_world_with_permissions:
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
                hello_world_with_permissions {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected
