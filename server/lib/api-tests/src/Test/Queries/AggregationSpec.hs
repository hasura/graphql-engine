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
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ]
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
        tableReferences = [Schema.reference "author" "author" "id"],
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

tests :: SpecWith TestEnvironment
tests = do
  describe "Aggregation queries" do
    it "Fetch aggregated data of an object" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          -- SQL Server returns int type when the input is of int type, which is different than the rest.
          -- https://learn.microsoft.com/en-us/sql/t-sql/functions/avg-transact-sql?view=sql-server-ver16#return-types
          avgResult :: String
          avgResult
            | fmap BackendType.backendType (getBackendTypeConfig testEnvironment) == Just Fixture.SQLServer = "3"
            | otherwise = "3.25"

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
                      rating: #{avgResult}
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

      shouldReturnYaml testEnvironment actual expected

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

      shouldReturnYaml testEnvironment actual expected

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

      shouldReturnYaml testEnvironment actual expected

    it "Fetch __typename for an aggregate" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      -- SQL Server returns int type when the input is of int type, which is different than the rest.
      -- https://learn.microsoft.com/en-us/sql/t-sql/functions/avg-transact-sql?view=sql-server-ver16#return-types
      let avgResult :: String
          avgResult
            | fmap BackendType.backendType (getBackendTypeConfig testEnvironment) == Just Fixture.SQLServer = "3"
            | otherwise = "3.25"

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_article_aggregate:
                  aggregate:
                    avg:
                      rating: #{avgResult}
                      __typename: #{schemaName}_article_avg_fields
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article_aggregate {
                    aggregate {
                      avg {
                        rating
                        __typename
                      }
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    -- This test checks an edge case where if aliasing is not done properly in the SQL, the column alias
    -- given to the nodes field can conflict with the column alias given to the column used in the aggregation
    -- which can break the query.
    it "Fetch aggregation over a column while also returning a field with the same name as the column" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_article_aggregate:
                  aggregate:
                    sum:
                      rating: 13
                  rating:
                    - id: 1
                    - id: 2
                    - id: 3
                    - id: 4
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article_aggregate(order_by: {id: asc}) {
                   aggregate {
                      sum {
                        rating
                      }
                    }
                    rating: nodes {
                      id
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
