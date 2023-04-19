{-# LANGUAGE QuasiQuotes #-}

-- | Test subscriptions over native queries
module Test.Queries.NativeQueries.SubscriptionsSpec (spec) where

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
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Subscriptions
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, shouldContain)

-- ** Preamble

featureFlagForNativeQueries :: String
featureFlagForNativeQueries = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForNativeQueries, "True")] $
    Fixture.run
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
  withSubscriptions do
    describe "A subscription on a native query" do
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

            articleWithExcerptLogicalModel :: Schema.LogicalModel
            articleWithExcerptLogicalModel =
              (Schema.logicalModel "article_with_excerpt")
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelScalar "id" Schema.TInt,
                      Schema.logicalModelScalar "title" Schema.TStr,
                      Schema.logicalModelScalar "excerpt" Schema.TStr,
                      Schema.logicalModelScalar "date" Schema.TUTCTime
                    ]
                }

            articleWithExcerptNativeQuery :: Schema.NativeQuery
            articleWithExcerptNativeQuery =
              (Schema.nativeQuery "article_with_excerpt" spicyQuery "article_with_excerpt")
                { Schema.nativeQueryArguments =
                    [Schema.nativeQueryColumn "length" Schema.TInt]
                }

        Schema.trackLogicalModel sourceName articleWithExcerptLogicalModel testEnvironment

        Schema.trackNativeQuery sourceName articleWithExcerptNativeQuery testEnvironment

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

          shouldReturnYaml testEnvironment actual expected

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

          shouldReturnYaml testEnvironment actual expected

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

          shouldReturnYaml testEnvironment actual expected

      it "multiplexes" $ \(mkSubscription, testEnvironment) -> do
        let backendTypeMetadata :: Fixture.BackendTypeConfig
            backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

            sourceName :: String
            sourceName = BackendType.backendSourceName backendTypeMetadata

            backendPrefix :: String
            backendPrefix = BackendType.backendTypeString backendTypeMetadata

            shouldBe :: IO Value -> Value -> IO ()
            shouldBe = shouldReturnYaml testEnvironment

            logicalModel :: Schema.LogicalModel
            logicalModel =
              (Schema.logicalModel "lm")
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelScalar "id" Schema.TInt,
                      Schema.logicalModelScalar "title" Schema.TStr,
                      Schema.logicalModelScalar "content" Schema.TStr,
                      Schema.logicalModelScalar "date" Schema.TUTCTime
                    ]
                }

            query :: Text
            query = [PG.sql| select * from article where title like {{pattern}} |]

            nativeQuery :: Schema.NativeQuery
            nativeQuery =
              (Schema.nativeQuery "filtered_article" query "lm")
                { Schema.nativeQueryArguments =
                    [Schema.nativeQueryColumn "pattern" Schema.TStr]
                }

        Schema.trackLogicalModel sourceName logicalModel testEnvironment
        Schema.trackNativeQuery sourceName nativeQuery testEnvironment

        one <- mkSubscription [graphql| subscription { filtered_article(args: { pattern: "%native%" }) { id, title } } |] []
        two <- mkSubscription [graphql| subscription { filtered_article(args: { pattern: "%model%" }) { id, title } } |] []

        getNextResponse one
          `shouldBe` [yaml|
            data:
              filtered_article: []
          |]

        getNextResponse two
          `shouldBe` [yaml|
            data:
              filtered_article: []
          |]

        _ <-
          GraphqlEngine.postV2Query
            200
            testEnvironment
            [interpolateYaml|
              type: #{backendPrefix}_run_sql
              args:
                cascade: false
                read_only: false
                source: #{sourceName}
                sql: |
                  insert into article (id, title, content, date) values
                    (1, 'I like the native song', '', now()),
                    (2, 'I like model trains', '', now()),
                    (3, 'I love me some native queries', '', now())
            |]

        getNextResponse one
          `shouldBe` [yaml|
            data:
              filtered_article:
              - id: 1
                title: I like the native song
              - id: 3
                title: I love me some native queries
          |]

        getNextResponse two
          `shouldBe` [yaml|
            data:
              filtered_article:
              - id: 2
                title: I like model trains
          |]
