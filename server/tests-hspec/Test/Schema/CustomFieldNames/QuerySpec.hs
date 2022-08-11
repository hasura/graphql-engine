{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests around queries involving fields with custom names.
--
-- https://hasura.io/docs/latest/schema/postgres/custom-field-names/
-- https://hasura.io/docs/latest/schema/ms-sql-server/custom-field-names/
-- https://hasura.io/docs/latest/schema/bigquery/custom-field-names/
module Test.Schema.CustomFieldNames.QuerySpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Test.Hspec (SpecWith, describe, it)
import Prelude

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.MySQL)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Mysql.setupTablesAction schema testEnv,
                  setupTeardown testEnv Fixture.MySQL
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  setupTeardown testEnv Fixture.Postgres
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv,
                  setupTeardown testEnv Fixture.Citus
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv,
                  setupTeardown testEnv Fixture.SQLServer
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
          [ Schema.column "prefix_id" Schema.TInt,
            Schema.column "prefix_name" Schema.TStr
          ],
        tablePrimaryKey = ["prefix_id"],
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

  describe "Queries using custom names" do
    it "Uses the custom field names in queries" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author:
                - id: 1
                  name: "Alice"
                - id: 2
                  name: "Bob"
            |]

          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author(order_by: [{ id: asc }]) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected

--------------------------------------------------------------------------------
-- Metadata

setupTeardown :: TestEnvironment -> Fixture.BackendType -> Fixture.SetupAction
setupTeardown testEnvironment backend = Fixture.SetupAction setupAction mempty
  where
    source = Fixture.defaultBackendTypeString backend
    config = source <> "_set_table_customization"

    setupAction =
      postMetadata_
        testEnvironment
        [yaml|
          type: *config
          args:
            source: *source
            table:
              schema: hasura
              name: author
            configuration:
              custom_column_names:
                "prefix_id": "id"
                "prefix_name": "name"
        |]
