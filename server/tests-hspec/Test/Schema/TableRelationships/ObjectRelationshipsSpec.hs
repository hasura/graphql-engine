{-# LANGUAGE QuasiQuotes #-}

-- |
-- Queries over object relationships between tables in the schema.
--
-- TODO: MySQL link when docs are released?
-- https://hasura.io/docs/latest/schema/postgres/table-relationships/index
-- https://hasura.io/docs/latest/schema/ms-sql-server/table-relationships/index
-- https://hasura.io/docs/latest/schema/bigquery/table-relationships/index/
module Test.Schema.TableRelationships.ObjectRelationshipsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.BackendType (BackendType (..))
import Harness.Test.Context (Options (..))
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec = do
  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.MySQL,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = Mysql.setup schema,
              teardown = Mysql.teardown schema,
              customOptions = Nothing
            }
        ]
    )
    $ tests MySQL

  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.Postgres,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = Postgres.setup schema,
              teardown = Postgres.teardown schema,
              customOptions = Nothing
            }
        ]
    )
    $ tests Postgres

  --   Context.run
  --     [ Context.Context
  --         { name = Context.Backend Context.Citus,
  --           mkLocalTestEnvironment = Context.noLocalTestEnvironment,
  --           setup = Citus.setup schema,
  --           teardown = Citus.teardown schema,
  --           customOptions = Nothing
  --         }
  --     ]
  --     $ tests Citus

  --   Context.run
  --     [ Context.Context
  --         { name = Context.Backend Context.SQLServer,
  --           mkLocalTestEnvironment = Context.noLocalTestEnvironment,
  --           setup = Sqlserver.setup schema,
  --           teardown = Sqlserver.teardown schema,
  --           customOptions = Nothing
  --         }
  --     ]
  --     $ tests SQLServer

  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.BigQuery,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = BigQuery.setup schema,
              teardown = BigQuery.teardown schema,
              customOptions =
                Just $
                  Context.Options
                    { stringifyNumbers = True
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
          [ Schema.Reference "author_id" "author" "id",
            Schema.Reference "co_author_id" "author" "id"
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

tests :: BackendType -> Context.Options -> SpecWith TestEnvironment
tests backend opts = describe "Object relationships" do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  it "Select articles and their authors" \testEnvironment -> do
    let expected :: Value
        expected =
          [yaml|
            data:
              hasura_article:
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
                hasura_article(order_by: [{ id: asc }]) {
                  id

                  author_by_author_id_to_id {
                    id
                  }
                }
              }
            |]

    actual `shouldBe` expected

  unless (backend `elem` [MySQL, BigQuery]) do
    describe "Null relationships" do
      it "Select articles their (possibly null) co-authors" \testEnvironment -> do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_article:
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
                    hasura_article(order_by: [{ id: asc }]) {
                      id

                      author_by_co_author_id_to_id {
                        id
                      }
                    }
                  }
                |]

        actual `shouldBe` expected
