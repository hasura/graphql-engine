{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Postgres-specific upsert feature.
-- https://github.com/hasura/graphql-engine/issues/8260
module Test.Regression.InsertOnConflict8260Spec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding (encodeUtf8)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Introspection (introspectEnums, introspectTypes)
import Harness.Test.Permissions (Permission (..))
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, shouldContain)

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BackendType.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  Postgres.setupPermissionsAction (permissions "postgres") testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend BackendType.Citus)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  Citus.setupPermissionsAction (permissions "citus") testEnvironment
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

permissions :: Text -> [Permission]
permissions source =
  [ SelectPermission
      { permissionTable = "foo",
        permissionSource = source,
        permissionRole = "role-select-only",
        permissionColumns = ["id", "bar"]
      },
    InsertPermission
      { permissionTable = "foo",
        permissionSource = source,
        permissionRole = "role-insert-only",
        permissionColumns = ["id", "bar"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

          actual `shouldBe` expected

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
                      hasura_foo {
                        bar
                        id
                      }
                    }
                  |]

          actual `shouldBe` expected
