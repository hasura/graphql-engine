{-# LANGUAGE QuasiQuotes #-}

-- |
-- Accessing objects based on their primary keys.
--
-- https://hasura.io/docs/latest/queries/postgres/simple-object-queries/
-- https://hasura.io/docs/latest/queries/ms-sql-server/simple-object-queries/
-- https://hasura.io/docs/latest/queries/bigquery/simple-object-queries/
module Test.Queries.Simple.PrimaryKeySpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Citus.setup schema,
          teardown = Citus.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Sqlserver.setup schema,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
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

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Primary key queries" do
    it "Lookup with primary key" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author_by_pk:
                  id: 1
                  name: Author 1
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author_by_pk(id: 1) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Lookup with (missing) primary key" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author_by_pk: null
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author_by_pk(id: 4) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected
