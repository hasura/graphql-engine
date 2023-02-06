{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Queries.NativeAccess.NativeAccessQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Database.PG.Query qualified as PG
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForNativeQuery :: String
featureFlagForNativeQuery = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForNativeQuery, "True")] $
    Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment
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
  [ (table "hello_world_table")
      { tableColumns =
          [ Schema.column "one" Schema.TStr,
            Schema.column "two" Schema.TStr
          ]
      },
    (table "article")
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
    (table "article_excerpt")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "excerpt" Schema.TStr,
            Schema.column "date" Schema.TUTCTime
          ]
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let query :: Text
      query = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")"

      shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Testing Native Access" $ do
    it "Runs a simple query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: hello_world_function
                code: *query
                returns:
                  name: hello_world_table
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

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

      actual `shouldBe` expected

    it "Runs a simple query that takes one dummy parameter" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: hello_world_function_with_dummy
                arguments:
                  dummy: varchar
                code: *query
                returns:
                  name: hello_world_table
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

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

      actual `shouldBe` expected

    it "Runs a simple query that takes no parameters but ends with a comment" $ \testEnvironment -> do
      let spicyQuery :: Text
          spicyQuery = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\") -- my query"

      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: hello_comment_function
                code: *spicyQuery
                returns:
                  name: hello_world_table
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

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

      actual `shouldBe` expected

    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  name: article_excerpt
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                article_with_excerpt(args: { length: "34" }) {
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
                    - id: 1
                      title: "Dogs"
                      date: "2000-01-01T00:00:00"
                      excerpt: "I like to eat dog food I am a dogs..."
              |]

      actual `shouldBe` expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt_1
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  name: article_excerpt
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt_2
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  name: article_excerpt
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                article_with_excerpt_1(args: { length: "34" }) {
                  excerpt
                }
                article_with_excerpt_2(args: { length: "13" }) {
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

      actual `shouldBe` expected

    it "Uses a one parameter query and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  name: article_excerpt
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                first: article_with_excerpt(args: { length: "34" }) {
                  excerpt
                }
                second: article_with_excerpt(args: { length: "13" }) {
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

      actual `shouldBe` expected

    it "Uses a one parameter query, passing it a GraphQL variable" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_native_query
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  name: article_excerpt
                  schema: *schemaName
            |]
        )
        [yaml|
          message: success
        |]

      let variables =
            [yaml|
              length: "34"
            |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithVariables
              testEnvironment
              [graphql|
                query MyQuery($length: int!) {
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

      actual `shouldBe` expected
