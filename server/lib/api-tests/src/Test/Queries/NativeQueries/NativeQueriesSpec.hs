{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Access to the SQL
module Test.Queries.NativeQueries.NativeQueriesSpec (spec, trackLogicalModels, postgresishTrackNativeQuery, schema, tests, distinctOnTests, basicNativeQuery) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Database.PG.Query qualified as PG
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction qualified as SetupAction
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig, graphQLTypeToText)
import Harness.Yaml (shouldAtLeastBe, shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Main Spec

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.hgeWithEnv [] do
    Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment,
                    trackLogicalModels testEnvironment,
                    postgresishTrackNativeQuery testEnvironment
                  ]
              }
          ]
      )
      (tests >> distinctOnTests)
    Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Sqlite.setupTablesAction schema testEnvironment,
                    trackLogicalModels testEnvironment,
                    sqliteTrackNativeQuery testEnvironment
                  ]
              }
          ]
      )
      tests

-- ** Fixtures

-- Includes Cockroach/Citus
postgresishTrackNativeQuery :: TestEnvironment -> SetupAction.SetupAction
postgresishTrackNativeQuery testEnvironment = SetupAction.noTeardown do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      source = BackendType.backendSourceName backendTypeMetadata

      helloNQ = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")"
      helloNQDups = "SELECT * FROM (VALUES ('hello', 'world'), ('hello', 'friend')) as t(\"one\", \"two\")"
      queryWithParam = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\") where 1={{param}}"
      articleNQ name =
        mkArticleWithExcerptNativeQuery
          name
          [PG.sql|
        select
          id,
          "Title",
          (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt
        from article
      |]

  -- Hello
  Schema.trackNativeQuery source (helloNQBasic helloNQ) testEnvironment
  Schema.trackNativeQuery source (helloNQWithDuplicates helloNQDups) testEnvironment
  Schema.trackNativeQuery source (uppercaseNativeQuery helloNQ) testEnvironment
  Schema.trackNativeQuery source (inlineNativeQuery helloNQ) testEnvironment
  Schema.trackNativeQuery source (helloSemicolonNQ helloNQ) testEnvironment
  Schema.trackNativeQuery source (helloCommentNQ helloNQ) testEnvironment
  -- Articles
  Schema.trackNativeQuery source (articleNQ "article_with_excerpt") testEnvironment
  Schema.trackNativeQuery source (articleNQ "article_with_excerpt_1") testEnvironment
  Schema.trackNativeQuery source (articleNQ "article_with_excerpt_2") testEnvironment
  -- Nullability
  Schema.trackNativeQuery source descriptionsAndNullableNativeQuery testEnvironment
  Schema.trackNativeQuery source allowedNullabilityNativeQuery testEnvironment
  Schema.trackNativeQuery source disallowedNullabilityNativeQuery testEnvironment
  -- Params
  Schema.trackNativeQuery source (helloNQWithParam queryWithParam) testEnvironment

sqliteTrackNativeQuery :: TestEnvironment -> SetupAction.SetupAction
sqliteTrackNativeQuery testEnvironment = SetupAction.noTeardown do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      source = BackendType.backendSourceName backendTypeMetadata

      -- Is there a nicer way to do this without UNION in SQLite?
      helloNQ = "SELECT 'hello' as one, 'world' as two UNION SELECT 'welcome' as one, 'friend' as two"
      helloNQDups = "SELECT 'hello' as one, 'world' as two UNION SELECT 'hello' as one, 'friend' as two"
      queryWithParam = "SELECT * FROM (SELECT 'hello' as one, 'world' as two UNION SELECT 'welcome' as one, 'friend' as two) WHERE 1={{param}}"
      articleNQ name =
        mkArticleWithExcerptNativeQuery
          name
          [PG.sql|
        select
          id,
          "Title",
          (substring(content, 1, {{length}}) || (CASE WHEN length(content) < {{length}} THEN '' ELSE '...' END)) as excerpt
        from article
      |]

  -- Hello
  Schema.trackNativeQuery source (helloNQBasic helloNQ) testEnvironment
  Schema.trackNativeQuery source (helloNQWithDuplicates helloNQDups) testEnvironment
  Schema.trackNativeQuery source (uppercaseNativeQuery helloNQ) testEnvironment
  Schema.trackNativeQuery source (inlineNativeQuery helloNQ) testEnvironment
  Schema.trackNativeQuery source (helloSemicolonNQ helloNQ) testEnvironment
  Schema.trackNativeQuery source (helloCommentNQ helloNQ) testEnvironment
  -- Articles
  Schema.trackNativeQuery source (articleNQ "article_with_excerpt") testEnvironment
  Schema.trackNativeQuery source (articleNQ "article_with_excerpt_1") testEnvironment
  Schema.trackNativeQuery source (articleNQ "article_with_excerpt_2") testEnvironment
  -- Nullability
  Schema.trackNativeQuery source descriptionsAndNullableNativeQuery testEnvironment
  Schema.trackNativeQuery source allowedNullabilityNativeQuery testEnvironment
  Schema.trackNativeQuery source disallowedNullabilityNativeQuery testEnvironment
  -- Params
  Schema.trackNativeQuery source (helloNQWithParam queryWithParam) testEnvironment

-- ** Logical Models

trackLogicalModels :: TestEnvironment -> Fixture.SetupAction
trackLogicalModels testEnvironment = SetupAction.noTeardown do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      source = BackendType.backendSourceName backendTypeMetadata

  Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment
  Schema.trackLogicalModel source articleWithExcerptLogicalModel testEnvironment
  Schema.trackLogicalModel source nullabilityLogicalModel testEnvironment
  Schema.trackLogicalModel source descriptionsAndNullableLogicalModel testEnvironment
  where
    nullabilityLogicalModel :: Schema.LogicalModel
    nullabilityLogicalModel =
      (Schema.logicalModel "nullability_model")
        { Schema.logicalModelColumns =
            [ Schema.logicalModelScalar "arbitrary_number" Schema.TInt
            ]
        }

    helloWorldLogicalModel :: Schema.LogicalModel
    helloWorldLogicalModel =
      (Schema.logicalModel "hello_world_return_type")
        { Schema.logicalModelColumns =
            [ Schema.logicalModelScalar "one" Schema.TStr,
              Schema.logicalModelScalar "two" Schema.TStr
            ]
        }

    articleWithExcerptLogicalModel :: Schema.LogicalModel
    articleWithExcerptLogicalModel =
      (Schema.logicalModel "article_with_excerpt")
        { Schema.logicalModelColumns =
            [ Schema.logicalModelScalar "id" Schema.TInt,
              Schema.logicalModelScalar "Title" Schema.TStr,
              Schema.logicalModelScalar "excerpt" Schema.TStr
            ]
        }

    descriptionsAndNullableLogicalModel :: Schema.LogicalModel
    descriptionsAndNullableLogicalModel =
      (Schema.logicalModel "divided_stuff")
        { Schema.logicalModelColumns =
            [ (Schema.logicalModelScalar "divided" Schema.TInt)
                { Schema.logicalModelColumnDescription = Just "A divided thing"
                },
              ( Schema.makeNullable
                  (Schema.logicalModelScalar "something_nullable" Schema.TStr)
                    { Schema.logicalModelColumnDescription = Just "Something nullable"
                    }
              )
            ],
          Schema.logicalModelDescription = Just "Return type description"
        }

-- ** Helpers

basicNativeQuery :: Text -> Text -> Text -> Schema.NativeQuery
basicNativeQuery name query returns = Schema.nativeQuery name (const query) returns

-- TODO: A lot of these "hello" function helpers are the same thing and can be generic
helloNQBasic :: Text -> Schema.NativeQuery
helloNQBasic sql = basicNativeQuery "hello_world_function" sql "hello_world_return_type"

helloSemicolonNQ :: Text -> Schema.NativeQuery
helloSemicolonNQ sql = basicNativeQuery "hello_semicolon_function" (sql <> "; \n") "hello_world_return_type"

helloCommentNQ :: Text -> Schema.NativeQuery
helloCommentNQ sql = basicNativeQuery "hello_comment_function" (sql <> " -- my query") "hello_world_return_type"

-- NOTE: This NQ isn't currently executed in any of the tests, it tests that missed parameters give useful errors
helloNQWithParam :: Text -> Schema.NativeQuery
helloNQWithParam sql =
  (Schema.nativeQuery "hello_world_function_with_arg" (const sql) "hello_world_return_type")
    { Schema.nativeQueryArguments =
        [Schema.nativeQueryColumn "param" Schema.TInt]
    }

inlineNativeQuery :: Text -> Schema.NativeQuery
inlineNativeQuery sql =
  ( Schema.inlineNativeQuery
      "hello_world_function_inline"
      (const sql)
      [ Schema.logicalModelScalar "one" Schema.TStr,
        Schema.logicalModelScalar "two" Schema.TStr
      ]
  )

helloNQWithDuplicates :: Text -> Schema.NativeQuery
helloNQWithDuplicates queryWithDuplicates =
  (Schema.nativeQuery "hello_world_function_duplicates" (const queryWithDuplicates) "hello_world_return_type")

-- The NQ Type `TInt` should be mapped to `INTEGER` via `backendScalarType :: ScalarType -> Text` field.
mkArticleWithExcerptNativeQuery :: Text -> Text -> Schema.NativeQuery
mkArticleWithExcerptNativeQuery name sql =
  (Schema.nativeQuery name (const sql) "article_with_excerpt")
    { Schema.nativeQueryArguments =
        [ Schema.nativeQueryColumn "length" Schema.TInt
        ]
    }

-- NOTE: May want to extend to other backends with SQL text param
descriptionsAndNullableNativeQuery :: Schema.NativeQuery
descriptionsAndNullableNativeQuery =
  Schema.nativeQuery "divided_stuff" (const nullableQuery) "divided_stuff"
  where
    nullableQuery = "SELECT (thing / 2)::integer AS divided, null::text as something_nullable FROM stuff"

disallowedNullabilityNativeQuery :: Schema.NativeQuery
disallowedNullabilityNativeQuery =
  (Schema.nativeQuery "non_nullability" (const nullabilityQuery) "nullability_model")
    { Schema.nativeQueryArguments =
        [ (Schema.nativeQueryColumn "index" Schema.TInt)
            { Schema.nativeQueryColumnNullable = False
            }
        ]
    }

-- NOTE: May want to extend to other backends with SQL text param
allowedNullabilityNativeQuery :: Schema.NativeQuery
allowedNullabilityNativeQuery =
  (Schema.nativeQuery "nullability" (const nullabilityQuery) "nullability_model")
    { Schema.nativeQueryArguments =
        [ (Schema.nativeQueryColumn "index" Schema.TInt)
            { Schema.nativeQueryColumnNullable = True
            }
        ]
    }

nullabilityQuery :: Text
nullabilityQuery = "SELECT coalesce({{index}}, 100) as arbitrary_number"

uppercaseNativeQuery :: Text -> Schema.NativeQuery
uppercaseNativeQuery sql = (Schema.nativeQuery "UppercaseNativeQuery" (const sql) "hello_world_return_type")

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
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "Title" Schema.TStr,
            Schema.column "content" Schema.TStr
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Dogs",
              Schema.VStr "I like to eat dog food I am a dogs I like to eat dog food I am a dogs I like to eat dog food I am a dogs"
            ]
          ]
      },
    (Schema.table "articles")
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
      },
    (Schema.table "authors")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tableData =
          [ [Schema.VInt 1, Schema.VStr "Marenghi"],
            [Schema.VInt 2, Schema.VStr "Learner"]
          ]
      }
  ]

-- ** Tests

-- | These should be defined seperately since some backends have issues with distinct-on.
--   We run these on everything except SQLServer, and SQLite because they don't have distinct_on implemented.
distinctOnTests :: SpecWith TestEnvironment
distinctOnTests = do
  describe "Distinct_on tests"
    $ it "Runs a simple query using distinct_on and order_by"
    $ \testEnvironment -> do
      let expected =
            [yaml|
                    data:
                      hello_world_function_duplicates:
                        - one: "hello"
                          two: "world"
                  |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
                      query {
                        hello_world_function_duplicates (
                          distinct_on: [one]
                          order_by: [{one:asc}]
                        ){
                          one
                          two
                        }
                      }
                   |]

      shouldReturnYaml testEnvironment actual expected

tests :: SpecWith TestEnvironment
tests = do
  describe "Testing Native Queries" $ do
    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                article_with_excerpt(args: { length: 34 }) {
                  id
                  Title
                  excerpt
                }
              }
           |]

          expected =
            [yaml|
                data:
                  article_with_excerpt:
                    - id: 1
                      Title: "Dogs"
                      excerpt: "I like to eat dog food I am a dogs..."
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
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
      let lengthType = graphQLTypeToText testEnvironment Schema.TInt
      let variables =
            [yaml|
              length: 34
            |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithVariables
              testEnvironment
              [graphql|
                query MyQuery($length: #{lengthType}!) {
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

    describe "Parameter nullability" $ do
      it "Query with non-nullable parameter does not accept null" $ \testEnvironment -> do
        actual <-
          GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
                query {
                  non_nullability(args: { index: null }) {
                    arbitrary_number
                  }
                }
             |]

        let expected =
              [yaml|
                  errors:
                  - extensions:
                      code: validation-failed
                      path: $.selectionSet.non_nullability.args.args.index
                |]

        actual `shouldAtLeastBe` expected

      it "Query with nullable parameter accepts a null value" $ \testEnvironment -> do
        let actual :: IO Value
            actual =
              GraphqlEngine.postGraphql
                testEnvironment
                [graphql|
                query {
                  notnull: nullability(args: { index: 42 }) {
                    arbitrary_number
                  }
                  withnull: nullability(args: { index: null }) {
                    arbitrary_number
                  }
               }
             |]

            expected =
              [yaml|
                  data:
                    notnull:
                      - arbitrary_number: 42
                    withnull:
                      - arbitrary_number: 100
                |]

        shouldReturnYaml testEnvironment actual expected

    it "Explain works" $ \testEnvironment -> do
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
      let intType = graphQLTypeToText testEnvironment Schema.TInt
      let stringType = graphQLTypeToText testEnvironment Schema.TStr
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
                            "name": "#{intType}"
                          }
                        }
                      },
                      {
                        "description": "Something nullable",
                        "name": "something_nullable",
                        "type": {
                          "kind": "SCALAR",
                          "name": "#{stringType}",
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

    it "Runs a simple query defined with an inline Logical Model" $ \testEnvironment -> do
      let expected =
            [yaml|
                data:
                  hello_world_function_inline:
                    - one: "hello"
                      two: "world"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function_inline(where: {one: {_eq: "hello"}}) {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Runs a simple query that takes no parameters" $ \testEnvironment -> do
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

    it "Runs a simple query that takes has an order_by" $ \testEnvironment -> do
      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - two: "world"
                    - two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function(order_by: {one: asc}) {
                  two
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Runs a simple query with a parameter that returns a nice error when we do not pass that parameter" $ \testEnvironment -> do
      let expected =
            [yaml|
                errors:
                  - extensions:
                      code: validation-failed
                      path: $.selectionSet.hello_world_function_with_arg.args.args.param
                    message: missing required field 'param'
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function_with_arg(args: {}, order_by: {one: asc}) {
                  two
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Runs a simple query that takes one dummy parameter and returns a nice error when we do not pass any args field" $ \testEnvironment -> do
      let expected =
            [yaml|
                errors:
                  - extensions:
                      code: validation-failed
                      path: $.selectionSet.hello_world_function_with_arg.args.args
                    message: missing required field 'args'
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function_with_arg(order_by: {one: asc}) {
                  two
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected

    it "Runs a simple query that takes no parameters but ends with a comment" $ \testEnvironment -> do
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

    it "Runs a simple query that takes no parameters but ends with a semicolon" $ \testEnvironment -> do
      let expected =
            [yaml|
                data:
                  hello_semicolon_function:
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
                hello_semicolon_function {
                  one
                  two
                }
              }
           |]

      shouldReturnYaml testEnvironment actual expected
