{-# LANGUAGE QuasiQuotes #-}

-- |
-- Offset queries
--
-- https://hasura.io/docs/latest/queries/postgres/pagination/
-- https://hasura.io/docs/latest/queries/ms-sql-server/pagination/
-- https://hasura.io/docs/latest/queries/bigquery/pagination/
module Test.Queries.Paginate.OffsetSpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context (Options (..))
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

spec :: SpecWith TestEnvironment
spec = do
  Context.run
    [ Context.Context
        { name = Context.Backend Context.MySQL,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Mysql.setup schema,
          teardown = Mysql.teardown schema,
          customOptions = Nothing
        },
      Context.Context
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
        },
      Context.Context
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
            [Schema.VInt 2, Schema.VStr "Author 2"],
            [Schema.VInt 3, Schema.VStr "Author 3"],
            [Schema.VInt 4, Schema.VStr "Author 4"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Paginate query results" do
    it "Offsets results by one element" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author:
                - name: Author 2
                  id: 2
                - name: Author 3
                  id: 3
                - name: Author 4
                  id: 4
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author(order_by: [{ id: asc }], offset: 1) {
                    name
                    id
                  }
                }
              |]

      actual `shouldBe` expected

    it "Correctly handles ordering, offsets, and limits" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author:
                - id: 2
                  name: Author 2
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author(limit: 1, offset: 2, order_by: [{ id: desc }]) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected
