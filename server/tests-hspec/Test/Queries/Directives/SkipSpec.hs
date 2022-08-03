{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for the GraphQL @skip query directive.
--
-- https://spec.graphql.org/June2018/#sec-Type-System.Directives
-- https://hasura.io/docs/latest/queries/postgres/variables-aliases-fragments-directives/
-- https://hasura.io/docs/latest/queries/ms-sql-server/variables-aliases-fragments-directives/
-- https://hasura.io/docs/latest/queries/bigquery/variables-aliases-fragments-directives/
module Test.Queries.Directives.SkipSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql, postGraphqlWithPair)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
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
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Skip fields conditionally" do
    it "Skips field with @skip(if: true)" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author:
                - name: Author 1
                - name: Author 2
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author(order_by: [{ id: asc }]) {
                    id @skip(if: true)
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Doesn't skip field with @skip(if: false)" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author:
                - id: 1
                  name: Author 1
                - id: 2
                  name: Author 2
            |]

          actual :: IO Value
          actual =
            postGraphqlWithPair
              testEnvironment
              [graphql|
                query test($skip: Boolean!) {
                  hasura_author(order_by: [{ id: asc }]) {
                    id @skip(if: $skip)
                    name
                  }
                }
              |]
              [ "variables" .= object ["skip" .= False]
              ]

      actual `shouldBe` expected
