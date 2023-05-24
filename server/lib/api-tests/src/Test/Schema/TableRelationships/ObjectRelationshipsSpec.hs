{-# LANGUAGE QuasiQuotes #-}

-- |
-- Queries over object relationships between tables in the schema.
--
-- https://hasura.io/docs/latest/schema/postgres/table-relationships/index
-- https://hasura.io/docs/latest/schema/ms-sql-server/table-relationships/index
-- https://hasura.io/docs/latest/schema/bigquery/table-relationships/index/
module Test.Schema.TableRelationships.ObjectRelationshipsSpec (spec) where

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
import Harness.Test.BackendType (BackendType (..))
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Postgres.setupTablesAction schema testEnv]
            }
        ]
    )
    $ tests Postgres

  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Citus.setupTablesAction schema testEnv]
            }
        ]
    )
    $ tests Citus

  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [Cockroach.setupTablesAction schema testEnv]
            }
        ]
    )
    $ tests Cockroach

  -- Fixture.run
  --   [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
  --       { Fixture.setupTeardown = \(testEnv, _) ->
  --           [Sqlserver.setupTablesAction schema testEnv]
  --       }
  --   ]
  --   $ tests SQLServer

  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [BigQuery.setupTablesAction schema testEnv],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            }
        ]
    )
    $ tests BigQuery

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
            Schema.column "author_id" Schema.TInt,
            Schema.columnNull "co_author_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ Schema.reference "author_id" "author" "id",
            Schema.reference "co_author_id" "author" "id"
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Article 1",
              Schema.VInt 1,
              Schema.VNull
            ],
            [ Schema.VInt 2,
              Schema.VStr "Article 2",
              Schema.VInt 1,
              Schema.VNull
            ],
            [ Schema.VInt 3,
              Schema.VStr "Article 3",
              Schema.VInt 2,
              Schema.VInt 1
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: BackendType -> SpecWith TestEnvironment
tests backend = describe "Object relationships" do
  it "Select articles and their authors" \testEnvironment -> do
    let schemaName = Schema.getSchemaName testEnvironment

    let expected :: Value
        expected =
          [interpolateYaml|
            data:
              #{schemaName}_article:
              - id: 1
                author_by_author_id_to_id:
                  id: 1

              - id: 2
                author_by_author_id_to_id:
                  id: 1

              - id: 3
                author_by_author_id_to_id:
                  id: 2
          |]

        -- We have to provide explicit orderings because BigQuery doesn't
        -- seem to return results in a deterministic order.
        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                #{schemaName}_article(order_by: [{ id: asc }]) {
                  id

                  author_by_author_id_to_id {
                    id
                  }
                }
              }
            |]

    shouldReturnYaml testEnvironment actual expected

  unless (backend == BigQuery) do
    describe "Null relationships" do
      it "Select articles their (possibly null) co-authors" \testEnvironment -> do
        let schemaName = Schema.getSchemaName testEnvironment

        let expected :: Value
            expected =
              [interpolateYaml|
                data:
                  #{schemaName}_article:
                  - id: 1
                    author_by_co_author_id_to_id: null
                  - id: 2
                    author_by_co_author_id_to_id: null
                  - id: 3
                    author_by_co_author_id_to_id:
                      id: 1
              |]

            actual :: IO Value
            actual =
              postGraphql
                testEnvironment
                [graphql|
                  query {
                    #{schemaName}_article(order_by: [{ id: asc }]) {
                      id

                      author_by_co_author_id_to_id {
                        id
                      }
                    }
                  }
                |]

        shouldReturnYaml testEnvironment actual expected
