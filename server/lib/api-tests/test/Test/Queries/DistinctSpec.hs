{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for @distinct@ queries.
--
-- https://hasura.io/docs/latest/queries/postgres/distinct-queries/
module Test.Queries.DistinctSpec where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
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
tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

    actual `shouldBe` expected

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

    actual `shouldBe` expected
