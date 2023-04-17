{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Databases.Postgres.NativeQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Database.PG.Query qualified as PG
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
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

tests :: SpecWith TestEnvironment
tests = do
  let articleQuery :: Text
      articleQuery =
        [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

  describe "Testing Native Queries" $ do
    let articleWithExcerptReturnType :: Schema.CustomReturnType
        articleWithExcerptReturnType =
          (Schema.customType "article_with_excerpt")
            { Schema.customTypeColumns =
                [ Schema.customReturnTypeScalar "id" Schema.TInt,
                  Schema.customReturnTypeScalar "title" Schema.TStr,
                  Schema.customReturnTypeScalar "excerpt" Schema.TStr,
                  Schema.customReturnTypeScalar "date" Schema.TUTCTime
                ]
            }

        mkArticleWithExcerptNativeQuery :: Text -> Schema.NativeQuery
        mkArticleWithExcerptNativeQuery name =
          (Schema.nativeQuery name articleQuery "article_with_excerpt")
            { Schema.nativeQueryArguments =
                [ Schema.nativeQueryColumn "length" Schema.TInt
                ]
            }

    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      Schema.trackCustomReturnType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery
        source
        (mkArticleWithExcerptNativeQuery "article_with_excerpt")
        testEnvironment

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
                    - id: 1
                      title: "Dogs"
                      date: "2000-01-01T00:00:00"
                      excerpt: "I like to eat dog food I am a dogs..."
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

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

      Schema.trackCustomReturnType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery
        source
        (mkArticleWithExcerptNativeQuery "article_with_excerpt")
        testEnvironment

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

      Schema.trackCustomReturnType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery
        source
        (mkArticleWithExcerptNativeQuery "article_with_excerpt")
        testEnvironment

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

    it "Runs a simple query using distinct_on and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          queryWithDuplicates :: Text
          queryWithDuplicates = "SELECT * FROM (VALUES ('hello', 'world'), ('hello', 'friend')) as t(\"one\", \"two\")"

          helloWorldReturnType :: Schema.CustomReturnType
          helloWorldReturnType =
            (Schema.customType "hello_world_function")
              { Schema.customTypeColumns =
                  [ Schema.customReturnTypeScalar "one" Schema.TStr,
                    Schema.customReturnTypeScalar "two" Schema.TStr
                  ]
              }

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
