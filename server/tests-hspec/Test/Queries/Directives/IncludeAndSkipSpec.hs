{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests for the interactions between @skip and @include query directives.
--
-- https://spec.graphql.org/June2018/#sec-Type-System.Directives
-- https://hasura.io/docs/latest/queries/postgres/variables-aliases-fragments-directives/
-- https://hasura.io/docs/latest/queries/ms-sql-server/variables-aliases-fragments-directives/
-- https://hasura.io/docs/latest/queries/bigquery/variables-aliases-fragments-directives/
module Test.Queries.Directives.IncludeAndSkipSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql, postGraphqlWithPair)
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
spec =
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
            [Schema.VInt 2, Schema.VStr "Author 2"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Mixes @include and @skip directives" do
    it "Returns the field when @include(if: true) and @skip(if: false)" \testEnvironment -> do
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
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author(order_by: [{ id: asc }]) {
                    id @include(if: true) @skip(if: false)
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Doesn't return the field when @include(if: false) and @skip(if: false)" \testEnvironment -> do
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
                    id @include(if: false) @skip(if: false)
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Doesn't return the field when @include(if: false) and @skip(if: true)" \testEnvironment -> do
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
                    id @include(if: false) @skip(if: true)
                    name
                  }
                }
              |]

      actual `shouldBe` expected

    it "Doesn't return the field when @include(if: true) and @skip(if: true)" \testEnvironment -> do
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
            postGraphqlWithPair
              testEnvironment
              [graphql|
                query test($skip: Boolean!, $include: Boolean!) {
                  hasura_author(order_by: [{ id: asc }]) {
                    id @include(if: $include) @skip(if: $skip)
                    name
                  }
                }
              |]
              ["variables" .= object ["skip" .= True, "include" .= True]]

      actual `shouldBe` expected
