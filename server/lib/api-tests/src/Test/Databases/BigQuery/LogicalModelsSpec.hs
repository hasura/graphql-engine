{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Databases.BigQuery.LogicalModelsSpec (spec) where

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
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (options), getBackendTypeConfig)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
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

      helloWorldLogicalModel :: Schema.LogicalModel
      helloWorldLogicalModel =
        (Schema.logicalModel "hello_world_function" query)
          { Schema.logicalModelColumns =
              [ Schema.logicalModelColumn "one" Schema.TStr,
                Schema.logicalModelColumn "two" Schema.TStr
              ]
          }

  describe "Testing Logical Models" $ do
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

    it "Runs a simple query using distinct_on and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          queryWithDuplicates :: Text
          queryWithDuplicates = "SELECT * FROM UNNEST([STRUCT('hello' as one, 'world' as two), ('hello', 'friend')])"

          helloWorldLogicalModelWithDuplicates :: Schema.LogicalModel
          helloWorldLogicalModelWithDuplicates =
            (Schema.logicalModel "hello_world_function" queryWithDuplicates)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldLogicalModelWithDuplicates testEnvironment

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
          spicyQuery = "SELECT * FROM UNNEST([STRUCT('hello' as one, 'world' as two), ('welcome', 'friend')]) -- my query"

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

  describe "Testing Logical Models" $ do
    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" (articleQuery schemaName))
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel source articleWithExcerptLogicalModel testEnvironment

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

      shouldReturnYaml (options testEnvironment) actual expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          mkArticleWithExcerptLogicalModel :: Text -> Schema.LogicalModel
          mkArticleWithExcerptLogicalModel name =
            (Schema.logicalModel name (articleQuery schemaName))
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel
        source
        (mkArticleWithExcerptLogicalModel "article_with_excerpt_1")
        testEnvironment

      Schema.trackLogicalModel
        source
        (mkArticleWithExcerptLogicalModel "article_with_excerpt_2")
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

      shouldReturnYaml (options testEnvironment) actual expected

    it "Uses a one parameter query and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" (articleQuery schemaName))
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel source articleWithExcerptLogicalModel testEnvironment

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

      shouldReturnYaml (options testEnvironment) actual expected

    it "Uses a one parameter query, passing it a GraphQL variable" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" (articleQuery schemaName))
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel source articleWithExcerptLogicalModel testEnvironment

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

      shouldReturnYaml (options testEnvironment) actual expected
