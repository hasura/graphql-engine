{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for @distinct@ queries.
--
-- https://hasura.io/docs/latest/queries/postgres/distinct-queries/
module Test.Queries.DistinctSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
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
          (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
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
            Schema.column "name" Schema.TStr,
            Schema.columnNull "genre" Schema.TStr,
            Schema.column "age" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Alice", Schema.VStr "Action", Schema.VInt 25],
            [Schema.VInt 2, Schema.VStr "Bob", Schema.VStr "Biography", Schema.VInt 40],
            [Schema.VInt 3, Schema.VStr "Carol", Schema.VStr "Crime", Schema.VInt 35],
            [Schema.VInt 4, Schema.VStr "Dave", Schema.VStr "Action", Schema.VInt 30],
            [Schema.VInt 5, Schema.VStr "Eve", Schema.VStr "Crime", Schema.VInt 25],
            [Schema.VInt 6, Schema.VStr "Bart", Schema.VNull, Schema.VInt 25]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

-- We specify the nulls ordering because the behaviour is not consistent between
-- postgres and cockroach.
--
-- For postgres and citus:
--   - @asc_nulls_last@ is the behaviour of @asc@
--   - @desc_nulls_first@ is the behaviour of @desc@
--
-- For cockroach, it's the opposite:
--   - @asc_nulls_first@ is the behaviour of @asc@
--   - @desc_nulls_last@ is the behaviour of @desc@
tests :: SpecWith TestEnvironment
tests = do
  it "Find the oldest writer of each genre - nulls last" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_author (
                  distinct_on: [genre],
                  order_by: [ { genre: asc_nulls_last }, { age: desc }, { id: asc } ]
                ) {
                  id
                  name
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_author:
              - id: 4
                name: Dave
              - id: 2
                name: Bob
              - id: 3
                name: Carol
              - id: 6
                name: Bart
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Find the oldest writer of each genre - nulls first" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_author (
                  distinct_on: [genre],
                  order_by: [ { genre: asc_nulls_first }, { age: desc }, { id: asc } ]
                ) {
                  id
                  name
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_author:
              - id: 6
                name: Bart
              - id: 4
                name: Dave
              - id: 2
                name: Bob
              - id: 3
                name: Carol
          |]

    shouldReturnYaml testEnvironment actual expected
