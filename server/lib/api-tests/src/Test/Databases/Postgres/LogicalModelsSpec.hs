{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Databases.Postgres.LogicalModelsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Database.PG.Query qualified as PG
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (options), getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
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

  describe "Testing Logical Models" $ do
    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" articleQuery)
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
                    - id: 1
                      title: "Dogs"
                      date: "2000-01-01T00:00:00"
                      excerpt: "I like to eat dog food I am a dogs..."
              |]

      shouldReturnYaml (options testEnvironment) actual expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          mkArticleWithExcerptLogicalModel :: Text -> Schema.LogicalModel
          mkArticleWithExcerptLogicalModel name =
            (Schema.logicalModel name articleQuery)
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

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" articleQuery)
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

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" articleQuery)
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
