{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Access to the SQL
module Test.Queries.NativeQueries.NativeQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (Permission (..), SelectPermissionDetails (..), selectPermission)
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (setupPermissionsAction)
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
                  [ Postgres.setupTablesAction schema testEnvironment,
                    setupPermissionsAction permissions testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Cockroach.setupTablesAction schema testEnvironment,
                    setupPermissionsAction permissions testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Citus.setupTablesAction schema testEnvironment,
                    setupPermissionsAction permissions testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Sqlserver.setupTablesAction schema testEnvironment,
                    setupPermissionsAction permissions testEnvironment
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
      },
    (Schema.table "article")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "author_id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr
          ],
        Schema.tableData =
          [ [Schema.VInt 1, Schema.VInt 1, Schema.VStr "Fright Knight", Schema.VStr "Well, well, well"],
            [Schema.VInt 2, Schema.VInt 2, Schema.VStr "Man to Man", Schema.VStr "Well2, well2, well2"]
          ]
      }
  ]

permissions :: [Permission]
permissions =
  [ SelectPermission
      selectPermission
        { selectPermissionTable = "article",
          selectPermissionRole = "sufficient",
          selectPermissionColumns = ["title", "author_id"]
        },
    SelectPermission
      selectPermission
        { selectPermissionTable = "article",
          selectPermissionRole = "insufficient",
          selectPermissionColumns = ["author_id"]
        }
  ]

tests :: SpecWith TestEnvironment
tests = do
  let query :: Text
      query = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")"

      helloWorldLogicalModel :: Schema.LogicalModel
      helloWorldLogicalModel =
        (Schema.logicalModel "hello_world_return_type")
          { Schema.logicalModelColumns =
              [ Schema.logicalModelScalar "one" Schema.TStr,
                Schema.logicalModelScalar "two" Schema.TStr
              ]
          }

      helloWorldNativeQuery :: Schema.NativeQuery
      helloWorldNativeQuery =
        (Schema.nativeQuery "hello_world_function" query "hello_world_return_type")

  describe "Testing Native Queries" $ do
    it "Explain works" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel sourceName helloWorldLogicalModel testEnvironment

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

          descriptionsAndNullableLogicalModel :: Schema.LogicalModel
          descriptionsAndNullableLogicalModel =
            (Schema.logicalModel "divided_stuff")
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelScalar "divided" Schema.TInt)
                      { Schema.logicalModelColumnDescription = Just "A divided thing"
                      },
                    (Schema.logicalModelScalar "something_nullable" Schema.TStr)
                      { Schema.logicalModelColumnDescription = Just "Something nullable",
                        Schema.logicalModelColumnNullable = True
                      }
                  ],
                Schema.logicalModelDescription = Just "Return type description"
              }

          descriptionsAndNullableNativeQuery :: Schema.NativeQuery
          descriptionsAndNullableNativeQuery =
            (Schema.nativeQuery "divided_stuff" nullableQuery "divided_stuff")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "unused" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel sourceName descriptionsAndNullableLogicalModel testEnvironment

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

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment
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

    it "Runs the a simple query with uppercase letters in the name" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          uppercaseNativeQuery :: Schema.NativeQuery
          uppercaseNativeQuery =
            (Schema.nativeQuery "UppercaseNativeQuery" query "hello_world_return_type")

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment
      Schema.trackNativeQuery source uppercaseNativeQuery testEnvironment

      let expected =
            [yaml|
                data:
                  UppercaseNativeQuery:
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
                UppercaseNativeQuery {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Runs simple query with a basic where clause" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment
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

      Schema.trackLogicalModel sourceName helloWorldLogicalModel testEnvironment
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

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

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

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

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
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_with_permissions" query "hello_world_return_type")

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

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
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_with_permissions" query "hello_world_return_type")

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

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
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_with_permissions" query "hello_world_return_type")

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

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

  describe "Native Query relationships" $ do
    let relationshipQuery :: Text
        relationshipQuery = "SELECT * FROM (VALUES (1, 'Marenghi'), (2, 'Learner')) as t(\"id\", \"name\")"

        articleLogicalModel :: Schema.LogicalModel
        articleLogicalModel =
          (Schema.logicalModel "article")
            { Schema.logicalModelColumns =
                [ Schema.logicalModelScalar "id" Schema.TInt,
                  Schema.logicalModelScalar "author_id" Schema.TInt,
                  Schema.logicalModelScalar "title" Schema.TStr,
                  Schema.logicalModelScalar "content" Schema.TStr
                ]
            }

        -- we'll need to add the `articles` relationship row later
        authorLogicalModel :: Schema.LogicalModel
        authorLogicalModel =
          (Schema.logicalModel "author")
            { Schema.logicalModelColumns =
                [ Schema.logicalModelScalar "id" Schema.TInt,
                  Schema.logicalModelScalar "name" Schema.TStr,
                  Schema.logicalModelReference "articles" "article"
                ]
            }

        -- broadly, a 'SELECT * FROM authors' type query
        relationshipNativeQuery :: Schema.NativeQuery
        relationshipNativeQuery =
          Schema.nativeQuery "relationship_test" relationshipQuery "author"

        nativeQueryWithRelationship :: String -> Schema.SchemaName -> Schema.NativeQuery
        nativeQueryWithRelationship schemaKeyword schemaName =
          let arrayRel =
                [interpolateYaml|
                      name: articles
                      using:
                        column_mapping:
                          id: author_id
                        insertion_order: null
                        remote_table:
                          name: article
                          #{schemaKeyword}: #{schemaName}
                  |]
           in relationshipNativeQuery
                { Schema.nativeQueryArrayRelationships = [arrayRel]
                }

    it "Adding a native query with a valid array relationship returns table data along with results for admin role" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

          schemaKeyword :: String
          schemaKeyword = Key.toString $ Fixture.backendSchemaKeyword backendTypeMetadata

      Schema.trackLogicalModel sourceName articleLogicalModel testEnvironment
      Schema.trackLogicalModel sourceName authorLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName (nativeQueryWithRelationship schemaKeyword schemaName) testEnvironment

      let expected =
            [yaml|
                data:
                  relationship_test:
                    - id: 1
                      name: "Marenghi"
                      articles:
                        - title: "Fright Knight"
                    - id: 2
                      name: "Learner"
                      articles:
                        - title: "Man to Man"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                relationship_test {
                  id
                  name
                  articles {
                    title
                  }
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Adding a native query with a valid array relationship fails when underlying table permissions are insufficient" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment
          backendType = BackendType.backendTypeString backendTypeMetadata

          schemaKeyword :: String
          schemaKeyword = Key.toString $ Fixture.backendSchemaKeyword backendTypeMetadata

      Schema.trackLogicalModel sourceName articleLogicalModel testEnvironment
      Schema.trackLogicalModel sourceName authorLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName (nativeQueryWithRelationship schemaKeyword schemaName) testEnvironment

      -- we're deliberately giving full permissions to the Logical Model -
      -- we're more interested in whether we're allowed to select from the
      -- `article` table
      void $
        GraphqlEngine.postMetadata
          testEnvironment
          [interpolateYaml|
              type: bulk
              args:
                - type: #{backendType}_create_logical_model_select_permission
                  args:
                    source: #{sourceName}
                    name: author 
                    role: "insufficient"
                    permission:
                      columns: "*" 
                      filter: {}
            |]

      let expected =
            [yaml|
                errors:
                  - extensions:
                      code: validation-failed
                      path: $.selectionSet.relationship_test
                    message: "field 'relationship_test' not found in type: 'query_root'"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "sufficient")
              ]
              [graphql|
              query {
                relationship_test {
                  id
                  name
                  articles {
                    title
                  }
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    -- I don't think this is the test we want - ideally checks of this kind
    -- would happen before the resolve the schema
    -- however for now let's just check that doing a stupid thing is not
    -- possible
    it "Native Query fails if we have not provided a way to fulfil a column of it's Logical Model" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel sourceName articleLogicalModel testEnvironment
      Schema.trackLogicalModel sourceName authorLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName relationshipNativeQuery testEnvironment

      let expected =
            [yaml|
                errors:
                  - extensions:
                      code: validation-failed
                      path: $.selectionSet.relationship_test
                    message: "field 'relationship_test' not found in type: 'query_root'"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                relationship_test {
                  id
                  name
                  articles {
                    title
                  }
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected
