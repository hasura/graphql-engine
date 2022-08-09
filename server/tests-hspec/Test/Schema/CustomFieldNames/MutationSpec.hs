{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests around mutations involving fields with custom names.
--
-- https://hasura.io/docs/latest/schema/postgres/custom-field-names/
-- https://hasura.io/docs/latest/schema/ms-sql-server/custom-field-names/
-- https://hasura.io/docs/latest/schema/bigquery/custom-field-names/
module Test.Schema.CustomFieldNames.MutationSpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.Citus qualified as Citus
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
    [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
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

  describe "Mutations using custom names" do
    it "Inserts" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_author:
                  returning:
                  - id: 3
                    name: "Eve"
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation author {
                  insert_hasura_author(objects: [
                    { id: 3
                      name: "Eve"
                    }
                  ]) {
                    returning {
                      id
                      name
                    }
                  }
                }
              |]

      actual `shouldBe` expected

    it "Updates" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                update_hasura_author:
                  returning:
                    - id: 3
                      name: 'Not Eve'
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation author {
                  update_hasura_author(
                    where: { id: { _eq: 3 } }
                    _set: { name: "Not Eve" }
                  ) {
                    returning {
                      id
                      name
                    }
                  }
                }
              |]

      actual `shouldBe` expected

    it "Deletes" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                delete_hasura_author:
                  returning:
                    - id: 3
                      name: "Not Eve"
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation author {
                  delete_hasura_author(
                    where: { id: { _eq: 3 } }
                  ) {
                    returning {
                      id
                      name
                    }
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
