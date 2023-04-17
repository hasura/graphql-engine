{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Access to the SQL
module Test.Databases.BigQuery.NativeQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Schema.Name (unSchemaName)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
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
          [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ BigQuery.setupTablesAction schema testEnvironment
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
  [ (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr,
            Schema.column "date" Schema.TUTCTime
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Dogs",
              Schema.VStr "I like to eat dog food I am a dogs I like to eat dog food I am a dogs I like to eat dog food I am a dogs",
              Schema.VUTCTime (UTCTime (fromOrdinalDate 2000 1) 0)
            ]
          ]
      },
    (table "stuff")
      { tableColumns =
          [ Schema.column "thing" Schema.TInt,
            Schema.column "date" Schema.TUTCTime
          ]
      }
  ]

tests :: SpecWith TestEnvironment
tests = do
  let query :: Text
      query = "SELECT * FROM UNNEST([STRUCT('hello' as one, 'world' as two), ('welcome', 'friend')])"

      helloWorldNativeQuery :: Schema.NativeQuery
      helloWorldNativeQuery =
        (Schema.nativeQuery "hello_world_function" query "hello_world_function")

      helloWorldReturnType :: Schema.CustomReturnType
      helloWorldReturnType =
        (Schema.customType "hello_world_function")
          { Schema.customTypeColumns =
              [ Schema.customReturnTypeScalar "one" Schema.TStr,
                Schema.customReturnTypeScalar "two" Schema.TStr
              ]
          }

      articleWithExcerptReturnType :: Schema.CustomReturnType
      articleWithExcerptReturnType =
        (Schema.customType "article_with_excerpt")
          { Schema.customTypeColumns =
              [ Schema.customReturnTypeScalar "id" Schema.TInt,
                Schema.customReturnTypeScalar "title" Schema.TStr,
                Schema.customReturnTypeScalar "excerpt" Schema.TStr,
                Schema.customReturnTypeScalar "date" Schema.TUTCTime
              ]
          }

  describe "Testing Native Queries" $ do
    it "Descriptions and nullability appear in the schema" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          nullableQuery = "SELECT thing / 2 AS divided, null as something_nullable FROM stuff"

          descriptionsAndNullableReturnType :: Schema.CustomReturnType
          descriptionsAndNullableReturnType =
            (Schema.customType "divided_stuff")
              { Schema.customTypeColumns =
                  [ (Schema.customReturnTypeScalar "divided" Schema.TInt)
                      { Schema.customReturnTypeColumnDescription = Just "A divided thing"
                      },
                    (Schema.customReturnTypeScalar "something_nullable" Schema.TInt)
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

    it "Runs a simple query using distinct_on and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          queryWithDuplicates :: Text
          queryWithDuplicates = "SELECT * FROM UNNEST([STRUCT('hello' as one, 'world' as two), ('hello', 'friend')])"

          helloWorldNativeQueryWithDuplicates :: Schema.NativeQuery
          helloWorldNativeQueryWithDuplicates =
            (Schema.nativeQuery "hello_world_function" queryWithDuplicates "hello_world_function")

      Schema.trackCustomReturnType source helloWorldReturnType testEnvironment

      Schema.trackNativeQuery source helloWorldNativeQueryWithDuplicates testEnvironment

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
                hello_world_function (
                  distinct_on: [one]
                  order_by: [{one:asc}]
                ){
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
            (Schema.nativeQuery "hello_world_function_with_dummy" query "hello_world_function")
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
          spicyQuery = "SELECT * FROM UNNEST([STRUCT('hello' as one, 'world' as two), ('welcome', 'friend')]) -- my query"

      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          helloCommentNativeQuery :: Schema.NativeQuery
          helloCommentNativeQuery =
            (Schema.nativeQuery "hello_comment_function" spicyQuery "hello_world_function")

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
            (Schema.nativeQuery "hello_world_perms" query "hello_world_function")

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
                    name: hello_world_function
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

      shouldReturnYaml testEnvironment actual expected

    it "Fails because we access a column we do not have permissions for" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_custom_return_type_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_perms" query "hello_world_function")

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
                    name: hello_world_function
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
                    message: "field 'one' not found in type: 'hello_world_function'"
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

      shouldReturnYaml testEnvironment actual expected

    it "Using row permissions filters out some results" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_custom_return_type_select_permission"

          helloWorldPermNativeQuery :: Schema.NativeQuery
          helloWorldPermNativeQuery =
            (Schema.nativeQuery "hello_world_perms" query "hello_world_function")

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
                    name: hello_world_function
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

      shouldReturnYaml testEnvironment actual expected

  let articleQuery :: Schema.SchemaName -> Text
      articleQuery schemaName =
        [i|
          SELECT
            id,
            title,
            ( substring(content, 1, {{length}}) || (
                case when length(content) < {{length}}
                  then ''
                  else '...' end
            )) as excerpt,
            date
            from #{unSchemaName schemaName}.article
        |]

  describe "Testing Native Queries" $ do
    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          articleWithExcerptNativeQuery :: Schema.NativeQuery
          articleWithExcerptNativeQuery =
            (Schema.nativeQuery "article_with_excerpt" (articleQuery schemaName) "article_with_excerpt")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "length" Schema.TInt
                  ]
              }

      Schema.trackCustomReturnType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery source articleWithExcerptNativeQuery testEnvironment

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                article_with_excerpt(args: { length: 34 }) {
                  id
                  title
                  date
                  excerpt
                }
              }
           |]

          expected =
            [yaml|
                data:
                  article_with_excerpt:
                    - id: '1'
                      title: "Dogs"
                      date: "2000-01-01T00:00:00"
                      excerpt: "I like to eat dog food I am a dogs..."
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          mkArticleWithExcerptNativeQuery :: Text -> Schema.NativeQuery
          mkArticleWithExcerptNativeQuery name =
            (Schema.nativeQuery name (articleQuery schemaName) "article_with_excerpt")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "length" Schema.TInt
                  ]
              }

      Schema.trackCustomReturnType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery
        source
        (mkArticleWithExcerptNativeQuery "article_with_excerpt_1")
        testEnvironment

      Schema.trackNativeQuery
        source
        (mkArticleWithExcerptNativeQuery "article_with_excerpt_2")
        testEnvironment

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                article_with_excerpt_1(args: { length: 34 }) {
                  excerpt
                }
                article_with_excerpt_2(args: { length: 13 }) {
                  excerpt
                }
              }
           |]

          expected =
            [yaml|
                data:
                  article_with_excerpt_1:
                    - excerpt: "I like to eat dog food I am a dogs..."
                  article_with_excerpt_2:
                    - excerpt: "I like to eat..."
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Uses a one parameter query and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          articleWithExcerptNativeQuery :: Schema.NativeQuery
          articleWithExcerptNativeQuery =
            (Schema.nativeQuery "article_with_excerpt" (articleQuery schemaName) "article_with_excerpt")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "length" Schema.TInt
                  ]
              }

      Schema.trackCustomReturnType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery source articleWithExcerptNativeQuery testEnvironment

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                first: article_with_excerpt(args: { length: 34 }) {
                  excerpt
                }
                second: article_with_excerpt(args: { length: 13 }) {
                  excerpt
                }
              }
           |]

          expected =
            [yaml|
                data:
                  first:
                    - excerpt: "I like to eat dog food I am a dogs..."
                  second:
                    - excerpt: "I like to eat..."
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Uses a one parameter query, passing it a GraphQL variable" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          articleWithExcerptNativeQuery :: Schema.NativeQuery
          articleWithExcerptNativeQuery =
            (Schema.nativeQuery "article_with_excerpt" (articleQuery schemaName) "article_with_excerpt")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "length" Schema.TInt
                  ]
              }

      Schema.trackCustomReturnType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery source articleWithExcerptNativeQuery testEnvironment

      let variables =
            [yaml|
              length: 34
            |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithVariables
              testEnvironment
              [graphql|
                query MyQuery($length: Int!) {
                  article_with_excerpt(args: { length: $length }) {
                    excerpt
                  }
                }
             |]
              variables

          expected =
            [yaml|
                data:
                  article_with_excerpt:
                    - excerpt: "I like to eat dog food I am a dogs..."
              |]

      shouldReturnYaml testEnvironment actual expected
