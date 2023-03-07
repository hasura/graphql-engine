{-# LANGUAGE QuasiQuotes #-}

-- | Test subscriptions over logical models
module Test.Queries.LogicalModels.SubscriptionsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Database.PG.Query qualified as PG
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Subscriptions
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, shouldContain)

-- ** Preamble

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

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
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  withSubscriptions do
    describe "A subscription on a logical model" do
      it "is updated on database changes" $ \(mkSubscription, testEnvironment) -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata
            backendPrefix = BackendType.backendTypeString backendTypeMetadata
            spicyQuery :: Text
            spicyQuery =
              [PG.sql|
                select
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
                  source: *sourceName
                  root_field_name: article_with_excerpt
                  code: *spicyQuery
                  arguments:
                    length:
                      type: int
                  returns:
                    columns:
                      id:
                        type: integer
                      title:
                        type: text
                      excerpt:
                        type: text
                      date:
                        type: date
              |]
          )
          [yaml|
            message: success
          |]

        query <-
          mkSubscription
            [graphql|
              subscription {
                article_with_excerpt(args: { length: "34" }) {
                  id
                  title
                  date
                  excerpt
                }
              }
            |]
            []
        -- check initial query result
        do
          let expected :: Value
              expected =
                [yaml|
                    data:
                      article_with_excerpt:
                        - id: 1
                          title: "Dogs"
                          date: "2000-01-01T00:00:00"
                          excerpt: "I like to eat dog food I am a dogs..."
                  |]
              actual :: IO Value
              actual = getNextResponse query

          actual `shouldBe` expected

        -- add a row
        do
          expected <-
            GraphqlEngine.postV2Query 200 testEnvironment $
              [interpolateYaml|
              type: #{backendPrefix}_run_sql
              args:
                cascade: false
                read_only: false
                source: #{sourceName}
                sql: |
                  insert into article values(
                    2,
                    'Cats',
                    'I like to eat cat food I am a cats I like to eat cat food I am a cats I like to eat cat food I am a cats',
                    '2000-01-01'
                  );
            |]
          show expected `shouldContain` "CommandOk"

        -- check updated response
        do
          let expected :: Value
              expected =
                [yaml|
                    data:
                      article_with_excerpt:
                        - id: 1
                          title: "Dogs"
                          date: "2000-01-01T00:00:00"
                          excerpt: "I like to eat dog food I am a dogs..."
                        - id: 2
                          title: "Cats"
                          date: "2000-01-01T00:00:00"
                          excerpt: "I like to eat cat food I am a cats..."
                  |]
              actual :: IO Value
              actual = getNextResponse query

          actual `shouldBe` expected

        -- delete a row
        do
          expected <-
            GraphqlEngine.postV2Query 200 testEnvironment $
              [interpolateYaml|
              type: #{backendPrefix}_run_sql
              args:
                cascade: false
                read_only: false
                source: #{sourceName}
                sql: |
                  delete from article where id = 2;
            |]
          show expected `shouldContain` "CommandOk"

        -- check updated response
        do
          let expected :: Value
              expected =
                [yaml|
                    data:
                      article_with_excerpt:
                        - id: 1
                          title: "Dogs"
                          date: "2000-01-01T00:00:00"
                          excerpt: "I like to eat dog food I am a dogs..."
                  |]
              actual :: IO Value
              actual = getNextResponse query

          actual `shouldBe` expected
