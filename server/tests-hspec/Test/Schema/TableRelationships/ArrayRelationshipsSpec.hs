{-# LANGUAGE QuasiQuotes #-}

module Test.Schema.TableRelationships.ArrayRelationshipsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
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
        [ (Fixture.fixture $ Fixture.Backend Fixture.MySQL)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Mysql.setupTablesAction schema testEnv]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Postgres.setupTablesAction schema testEnv]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Citus.setupTablesAction schema testEnv]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Sqlserver.setupTablesAction schema testEnv]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.BigQuery)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [BigQuery.setupTablesAction schema testEnv],
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
          [ [Schema.VInt 1, Schema.VStr "Author 1"],
            [Schema.VInt 2, Schema.VStr "Author 2"]
          ]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "author_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ Schema.Reference "author_id" "author" "id"
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Article 1",
              Schema.VInt 1
            ],
            [ Schema.VInt 2,
              Schema.VStr "Article 2",
              Schema.VInt 1
            ],
            [ Schema.VInt 3,
              Schema.VStr "Article 3",
              Schema.VInt 2
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

  describe "Array relationships" do
    it "Select authors and their articles" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_author:
                - id: 1
                  articles_by_id_to_author_id:
                  - id: 1
                  - id: 2

                - id: 2
                  articles_by_id_to_author_id:
                  - id: 3
            |]

          -- We have to provide explicit orderings because BigQuery doesn't
          -- seem to return results in a deterministic order.
          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_author(order_by: [{ id: asc }]) {
                    id

                    articles_by_id_to_author_id(order_by: [{ id: asc }]) {
                      id
                    }
                  }
                }
              |]

      actual `shouldBe` expected
