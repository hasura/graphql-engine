{-# LANGUAGE QuasiQuotes #-}

-- |
-- Queries involving the `operationName` key.
--
-- https://spec.graphql.org/June2018/#sec-Executing-Requests
module Test.Queries.Simple.OperationNameSpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphqlYaml)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Context (Options (..))
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.Test.SchemaName
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

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
tests opts = describe "BasicFieldsSpec" do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Use the `operationName` key" do
    it "Selects the correct operation" \testEnvironment -> do
      let schemaName = getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - name: Author 1
                  id: 1
                - name: Author 2
                  id: 2
            |]

          actual :: IO Value
          actual =
            postGraphqlYaml
              testEnvironment
              [interpolateYaml|
                operationName: chooseThisOne
                query: |
                  query ignoreThisOne {
                    MyQuery {
                      name
                    }
                  }
                  query chooseThisOne {
                    #{schemaName}_author(order_by:[{id:asc}]) {
                      id
                      name
                    }
                  }
              |]

      actual `shouldBe` expected
