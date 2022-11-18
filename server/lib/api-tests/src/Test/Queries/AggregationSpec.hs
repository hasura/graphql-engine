{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for GraphQL aggregation queries.
--
-- https://hasura.io/docs/latest/queries/postgres/aggregation-queries/
-- https://hasura.io/docs/latest/queries/ms-sql-server/aggregation-queries/
-- https://hasura.io/docs/latest/queries/bigquery/aggregation-queries/
module Test.Queries.AggregationSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True,
                      Fixture.skipTests = Just "BigQuery returns numbers as strings, which means the second test fails"
                    }
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
                    { Fixture.skipTests = Just "SQL Server currently rounds the average - we think this is a bug. Ref https://hasurahq.atlassian.net/browse/NDAT-335"
                    }
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "author")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Author 1"],
            [Schema.VInt 2, Schema.VStr "Author 2"]
          ]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "rating" Schema.TInt,
            Schema.column "author" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences = [Schema.Reference "author" "author" "id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Article 1", Schema.VInt 1, Schema.VInt 1],
            [Schema.VInt 2, Schema.VStr "Article 2", Schema.VInt 4, Schema.VInt 2],
            [Schema.VInt 3, Schema.VStr "Article 3", Schema.VInt 3, Schema.VInt 2],
            [Schema.VInt 4, Schema.VStr "Article 4", Schema.VInt 5, Schema.VInt 2]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Aggregation queries" do
    it "Fetch aggregated data of an object" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_article_aggregate:
                  aggregate:
                    count: 4
                    sum:
                      rating: 13
                    avg:
                      rating: 3.25
                    max:
                      rating: 5
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article_aggregate {
                    aggregate {
                      count
                      sum {
                        rating
                      }
                      avg {
                        rating
                      }
                      max {
                        rating
                      }
                    }
                  }
                }
              |]

      actual `shouldBe` expected

    it "Fetch aggregated data on nested objects" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author (where: {id: {_eq: 1}}) {
                    id
                    name
                    articles_by_id_to_author_aggregate {
                      aggregate {
                        count
                        avg {
                          rating
                        }
                        max {
                          rating
                        }
                      }
                      nodes {
                        id
                        title
                        rating
                      }
                    }
                  }
                }
              |]

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
               #{schemaName}_author:
               - articles_by_id_to_author_aggregate:
                   aggregate:
                     avg:
                       rating: 1
                     count: 1
                     max:
                       rating: 1
                   nodes:
                   - id: 1
                     rating: 1
                     title: Article 1
                 id: 1
                 name: Author 1
            |]

      actual `shouldBe` expected

    it "Fetch aggregated count only" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_article_aggregate:
                  aggregate:
                    count: 4
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article_aggregate {
                   aggregate {
                      count
                    }
                  }
                }
              |]

      actual `shouldBe` expected
