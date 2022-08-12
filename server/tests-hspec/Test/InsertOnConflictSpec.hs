{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Postgres-specific upsert feature.
module Test.InsertOnConflictSpec (spec) where

import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture
import Harness.Test.Introspection
import Harness.Test.Permissions (Permission (..))
import Harness.Test.Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = run [postgresFixture, citusFixture] tests

postgresFixture :: Fixture ()
postgresFixture =
  (fixture $ Backend BackendType.Postgres)
    { setupTeardown = \(t, _) ->
        [ Postgres.setupTablesAction tables t,
          Postgres.setupPermissionsAction (permissions "postgres") t
        ]
    }

citusFixture :: Fixture ()
citusFixture =
  (fixture $ Backend BackendType.Citus)
    { setupTeardown = \(t, _) ->
        [ Citus.setupTablesAction tables t,
          Citus.setupPermissionsAction (permissions "citus") t
        ]
    }

tables :: [Table]
tables =
  [ (table "foo")
      { tableColumns =
          [ column "id" TInt,
            column "bar" TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [VInt 0, VStr "initial"]
          ]
      }
  ]

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

tests :: Options -> SpecWith TestEnvironment
tests _ =
  -- Tests relating to https://github.com/hasura/graphql-engine/issues/8260
  describe "The schema for insert mutations with an 'on_conflict' clause" do
    describe "When no columns are updateable" do
      it "Is still present with an empty enum" testEmptyUpdateColumns
      it "Inserts ignoring duplicates" testInsertDoNothing

testEmptyUpdateColumns :: TestEnvironment -> IO ()
testEmptyUpdateColumns env = do
  introspectTypes env "role-insert-only"
    >>= (`shouldContain` ["hasura_foo_on_conflict"])

  introspectEnums env "role-insert-only"
    >>= (`shouldContain` [("hasura_foo_update_column", ["_PLACEHOLDER"])])

testInsertDoNothing :: TestEnvironment -> IO ()
testInsertDoNothing env = do
  -- We can insert ignoring duplicates
  GraphqlEngine.postGraphqlWithHeaders
    env
    [("X-Hasura-Role", encodeUtf8 "role-insert-only")]
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
    >>= ( `shouldBe`
            [yaml|
              data:
                insert_hasura_foo:
                  affected_rows: 1
            |]
        )

  -- The data actually gets stored
  GraphqlEngine.postGraphqlWithHeaders
    env
    [("X-Hasura-Role", encodeUtf8 "role-select-only")]
    [graphql|
      query ActualData {
        hasura_foo {
          bar
          id
        }
      }
      |]
    >>= ( `shouldBe`
            [yaml|
              data:
                hasura_foo:
                - bar: "initial"
                  id: 0
                - bar: "inserted"
                  id: 1
            |]
        )
