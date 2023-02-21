{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Queries.LogicalModels.LogicalModelsQueriesSpec (spec) where

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

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] $
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
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let query :: Text
      query = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")"

      shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Testing Logical Models" $ do
    it "Runs the absolute simplest query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: hello_world_function
                code: *query
                returns:
                  columns:
                    one: text
                    two: text
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

      actual `shouldBe` expected

    it "Runs simple query with a basic where clause" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: hello_world_function
                code: *query
                returns:
                  columns:
                    one: text
                    two: text
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
                hello_world_function (where: { two: { _eq: "world" } }){
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs a simple query using distinct_on and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          queryWithDuplicates :: Text
          queryWithDuplicates = "SELECT * FROM (VALUES ('hello', 'world'), ('hello', 'friend')) as t(\"one\", \"two\")"

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: hello_world_function
                code: *queryWithDuplicates
                returns:
                  columns:
                    one: text
                    two: text
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
                hello_world_function (
                  distinct_on: [one]
                  order_by: [{one:asc}]
                ){
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs a simple query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: hello_world_function
                code: *query
                returns:
                  columns:
                    one: text
                    two: text
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

    it "Runs a simple query that takes one dummy parameter and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: hello_world_function_with_dummy
                arguments:
                  dummy: varchar
                code: *query
                returns:
                  columns:
                    one: text
                    two: text
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

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: hello_comment_function
                code: *spicyQuery
                returns:
                  columns:
                    one: text
                    two: text
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
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  columns:
                    id: integer
                    title: text
                    excerpt: text
                    date: date
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
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt_1
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  columns:
                    id: integer
                    title: text
                    excerpt: text
                    date: date
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
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt_2
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  columns:
                    id: integer
                    title: text
                    excerpt: text
                    date: date
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
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  columns:
                    id: integer
                    title: text
                    excerpt: text
                    date: date
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
              type: pg_track_logical_model
              args:
                type: query
                source: *source
                root_field_name: article_with_excerpt
                code: *spicyQuery
                arguments:
                  length: int
                returns:
                  columns:
                    id: integer
                    title: text
                    excerpt: text
                    date: date
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
