{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Postgres-specific upsert feature.
-- https://github.com/hasura/graphql-engine/issues/8260
module Test.Regression.InsertOnConflict8260Spec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding (encodeUtf8)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders)
import Harness.Permissions (InsertPermissionDetails (..), Permission (..), SelectPermissionDetails (..), insertPermission, selectPermission)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Introspection (introspectEnums, introspectTypes)
import Harness.Test.SetupAction (setupPermissionsAction)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, shouldContain)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupPermissionsAction permissions testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupPermissionsAction permissions testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupPermissionsAction permissions testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "foo")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "bar" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData =
          [ [ Schema.VInt 0,
              Schema.VStr "initial"
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Permissions

permissions :: [Permission]
permissions =
  [ SelectPermission
      selectPermission
        { selectPermissionTable = "foo",
          selectPermissionRole = "role-select-only",
          selectPermissionColumns = ["id", "bar"]
        },
    InsertPermission
      insertPermission
        { insertPermissionTable = "foo",
          insertPermissionRole = "role-insert-only",
          insertPermissionColumns = ["id", "bar"]
        }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  describe "The schema for insert mutations with an 'on_conflict' clause" do
    describe "When no columns are updateable" do
      it "Is still present with an empty enum" \testEnvironment -> do
        types <- introspectTypes testEnvironment "role-insert-only"
        types `shouldContain` ["hasura_foo_on_conflict"]

        enums <- introspectEnums testEnvironment "role-insert-only"
        enums `shouldContain` [("hasura_foo_update_column", ["_PLACEHOLDER"])]

      it "Inserts ignoring duplicates" \testEnvironment -> do
        do
          let expected :: Value
              expected =
                [yaml|
                  data:
                    insert_hasura_foo:
                      affected_rows: 1
                |]

              actual :: IO Value
              actual =
                postGraphqlWithHeaders
                  testEnvironment
                  [ ("X-Hasura-Role", encodeUtf8 "role-insert-only")
                  ]
                  [graphql|
                      mutation OnConflictDoNothing {
                        insert_hasura_foo
                        (
                          objects: [
                            {bar: "untouched", id: 0},
                            {bar: "inserted",  id: 1}],
                          on_conflict: {constraint: foo_pkey, update_columns: []}
                        )
                        {
                          affected_rows
                        }
                      }
                    |]

          shouldReturnYaml testEnvironment actual expected

        do
          let expected :: Value
              expected =
                [yaml|
                  data:
                    hasura_foo:
                    - bar: "initial"
                      id: 0
                    - bar: "inserted"
                      id: 1
                |]

              actual :: IO Value
              actual =
                postGraphqlWithHeaders
                  testEnvironment
                  [("X-Hasura-Role", encodeUtf8 "role-select-only")]
                  [graphql|
                    query ActualData {
                      hasura_foo(order_by: {id: asc}) {
                        bar
                        id
                      }
                    }
                  |]

          shouldReturnYaml testEnvironment actual expected
