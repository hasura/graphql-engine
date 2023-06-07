{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Tests around mutations involving fields with custom names.
--
-- https://hasura.io/docs/latest/schema/postgres/custom-field-names/
-- https://hasura.io/docs/latest/schema/ms-sql-server/custom-field-names/
-- https://hasura.io/docs/latest/schema/bigquery/custom-field-names/
module Test.Schema.CustomFieldNames.MutationSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Test.Hspec (SpecWith, describe, it)
import Prelude

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.runSingleSetup
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  setupTeardown testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv,
                  setupTeardown testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv,
                  setupTeardown testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv,
                  setupTeardown testEnv
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

tests :: SpecWith TestEnvironment
tests =
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

      shouldReturnYaml testEnvironment actual expected

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

      shouldReturnYaml testEnvironment actual expected

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

      shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- Metadata

setupTeardown :: TestEnvironment -> Fixture.SetupAction
setupTeardown testEnvironment = Fixture.SetupAction setupAction mempty
  where
    backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    backendPrefix = Fixture.backendTypeString backendTypeMetadata
    source = Fixture.backendSourceName backendTypeMetadata
    config = backendPrefix <> "_set_table_customization"

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
