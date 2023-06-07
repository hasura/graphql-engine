{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for the GraphQL @include query directive.
--
-- https://spec.graphql.org/June2018/#sec-Type-System.Directives
-- https://hasura.io/docs/latest/queries/postgres/variables-aliases-fragments-directives/
-- https://hasura.io/docs/latest/queries/ms-sql-server/variables-aliases-fragments-directives/
-- https://hasura.io/docs/latest/queries/bigquery/variables-aliases-fragments-directives/
module Test.Queries.VariablesAliasesFragments.Directives.IncludeSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql, postGraphqlWithPair)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

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
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
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
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction schema testEnvironment
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
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  describe "Include fields conditionally" do
    it "Includes field with @include(if: true)" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
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
                query test($include: Boolean!) {
                  #{schemaName}_author(order_by: [{ id: asc }]) {
                    id @include(if: $include)
                    name
                  }
                }
              |]
              ["variables" .= object ["include" .= True]]

      shouldReturnYaml testEnvironment actual expected

    it "Doesn't include field with @include(if: false)" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - name: Author 1
                - name: Author 2
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author(order_by: [{ id: asc }]) {
                    id @include(if: false)
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
