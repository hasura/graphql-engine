{-# LANGUAGE QuasiQuotes #-}

-- |
-- Simple queries on single objects.
--
-- https://hasura.io/docs/latest/queries/postgres/simple-object-queries/
-- https://hasura.io/docs/latest/queries/ms-sql-server/simple-object-queries/
-- https://hasura.io/docs/latest/queries/bigquery/simple-object-queries/
module Test.Queries.Simple.ObjectQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
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
        [ (Fixture.fixture $ Fixture.Backend Fixture.MySQL)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Mysql.setupTablesAction schema testEnv
                ]
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
          (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.Options
                    { stringifyNumbers = True
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
          [ [ Schema.VInt 1,
              Schema.VStr "Author 1"
            ],
            [ Schema.VInt 2,
              Schema.VStr "Author 2"
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Simple object queries" do
    it "Fetch a list of authors" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - id: 1
                  name: "Author 1"
                - id: 2
                  name: "Author 2"
            |]

          -- We have to set an ordering for BigQuery, as return order isn't
          -- guaranteed.
          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author(order_by: [{ id: asc }]) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Removes `no_queries_available` when queries are available" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              errors:
              - extensions:
                  code: validation-failed
                  path: $.selectionSet.no_queries_available
                message: |-
                  field 'no_queries_available' not found in type: 'query_root'
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  no_queries_available
                }
              |]

      actual `shouldBe` expected

    it "Fails on missing tables" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              errors:
              - extensions:
                  code: validation-failed
                  path: $.selectionSet.random
                message: |-
                  field 'random' not found in type: 'query_root'
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  random {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Fails on missing fields" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              errors:
              - extensions:
                  code: validation-failed
                  path: $.selectionSet.#{schemaName}_author.selectionSet.notPresentCol
                message: |-
                  field 'notPresentCol' not found in type: '#{schemaName}_author'
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author {
                    id
                    name
                    notPresentCol
                  }
                }
              |]

      actual `shouldBe` expected
