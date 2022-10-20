{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for queries involving GraphQL variables.
--
-- https://hasura.io/docs/latest/queries/postgres/variables-aliases-fragments-directives/#using-variables
-- https://hasura.io/docs/latest/queries/ms-sql-server/variables-aliases-fragments-directives/#using-variables
-- https://hasura.io/docs/latest/queries/bigquery/variables-aliases-fragments-directives/#using-variables
module Test.Queries.VariablesSpec where

import Data.Aeson (Value (String), object, (.=))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphqlWithPair)
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
                  Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
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
          [ [Schema.VInt 1, Schema.VStr "Alice"],
            [Schema.VInt 2, Schema.VStr "Bob"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Using variables" do
    it "A query involving a single variable" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

          actual :: IO Value
          actual =
            postGraphqlWithPair
              testEnvironment
              [graphql|
                query($author: String!) {
                  #{schemaName}_author (where: { name: { _eq: $author } }) {
                    name
                    id
                  }
                }
              |]
              [ "variables"
                  .= object
                    [ ("author", String "Alice")
                    ]
              ]

          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - name: Alice
                  id: 1
            |]

      actual `shouldBe` expected
