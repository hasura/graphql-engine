{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Regression.OptimizePermissionFiltersSpec (spec) where

import Data.Aeson
import Data.Has
import Harness.GlobalTestEnvironment
import Harness.Logging
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Services.GraphqlEngine
import Harness.Services.Permissions.Table.Postgres
import Harness.Services.Schema
import Harness.Services.Source.Postgres
import Harness.Yaml
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec = do
  withHge emptyHgeConfig $ do
    withPostgresSource "postgres-source"
      $ withSchemaName "test_schema"
      $ withPostgresSchema schema
      $ withTablePermissions permissions
      $ testUnoptimized

  withHge
    ( emptyHgeConfig
        { hgeConfigEnvironmentVars =
            [("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "optimize_permission_filters")]
        }
    )
    $ do
      withPostgresSource "postgres-source"
        $ withSchemaName "test_schema"
        $ withPostgresSchema schema
        $ withTablePermissions permissions
        $ testOptimized

permissions ::
  ( Has PostgresSource env,
    Has Schema.SchemaName env
  ) =>
  env ->
  [Permission]
permissions env =
  [ SelectPermission
      $ (selectPermission env "foo")
        { selectPermissionColumns = ["foo_id"],
          selectPermissionRows =
            [yaml| 
                  foo_id:
                    _lte: 42
                  |]
        },
    SelectPermission
      $ (selectPermission env "bar")
        { selectPermissionColumns = ["bar_id"],
          selectPermissionRows =
            [yaml| 
                  foo_by_bar_foo_id_to_foo_id:
                    foo_id:
                      _lte: 42
                  |]
        }
  ]

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "foo")
      { Schema.tableColumns =
          [ Schema.column "foo_id" Schema.TInt
          ],
        Schema.tablePrimaryKey = ["foo_id"],
        Schema.tableData = []
      },
    (Schema.table "bar")
      { Schema.tableColumns =
          [ Schema.column "bar_id" Schema.TInt,
            Schema.column "bar_foo_id" Schema.TInt
          ],
        Schema.tablePrimaryKey = ["bar_id"],
        Schema.tableReferences = [Schema.reference "bar_foo_id" "foo" "foo_id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

testOptimized ::
  forall env.
  ( Has HgeServerInstance env,
    Has Logger env,
    Has Schema.SchemaName env
  ) =>
  SpecWith env
testOptimized = do
  it "Produces SQL without superfluous permission checks" \env -> do
    let expected :: Value
        expected =
          [yaml|
           - field: test_schema_foo
             sql: |
                SELECT
                  coalesce(json_agg("root"), '[]') AS "root"
                FROM
                  (
                    SELECT
                      row_to_json(
                        (
                          SELECT
                            "_e"
                          FROM
                            (
                              SELECT
                                "_root.base"."foo_id" AS "foo_id",
                                "_root.ar.root.bars_by_foo_id_to_bar_foo_id"."bars_by_foo_id_to_bar_foo_id" AS "bars_by_foo_id_to_bar_foo_id"
                            ) AS "_e"
                        )
                      ) AS "root"
                    FROM
                      (
                        SELECT
                          *
                        FROM
                          "test_schema"."foo"
                        WHERE
                          (
                            ("test_schema"."foo"."foo_id") <= (('42') :: integer)
                          )
                      ) AS "_root.base"
                      LEFT OUTER JOIN LATERAL (
                        SELECT
                          coalesce(json_agg("bars_by_foo_id_to_bar_foo_id"), '[]') AS "bars_by_foo_id_to_bar_foo_id"
                        FROM
                          (
                            SELECT
                              row_to_json(
                                (
                                  SELECT
                                    "_e"
                                  FROM
                                    (
                                      SELECT
                                        "_root.ar.root.bars_by_foo_id_to_bar_foo_id.base"."bar_id" AS "bar_id"
                                    ) AS "_e"
                                )
                              ) AS "bars_by_foo_id_to_bar_foo_id"
                            FROM
                              (
                                SELECT
                                  *
                                FROM
                                  "test_schema"."bar"
                                WHERE
                                  (("_root.base"."foo_id") = ("bar_foo_id"))
                              ) AS "_root.ar.root.bars_by_foo_id_to_bar_foo_id.base"
                          ) AS "_root.ar.root.bars_by_foo_id_to_bar_foo_id"
                      ) AS "_root.ar.root.bars_by_foo_id_to_bar_foo_id" ON ('true')
                  ) AS "_root"
          |]

        schemaName = getter @Schema.SchemaName env

    -- print (getHgeServerInstanceUrl $ getter env)
    -- void getLine

    actual <-
      hgePostExplainRole
        env
        "test-role"
        [graphql|
        query
          {
            #{schemaName}_foo
              {
                foo_id
                bars_by_foo_id_to_bar_foo_id
                {
                  bar_id
                }
              }
          }
        |]

    ignoreWhitespace actual `shouldAtLeastBe` ignoreWhitespace expected

testUnoptimized ::
  ( Has HgeServerInstance env,
    Has Logger env,
    Has Schema.SchemaName env
  ) =>
  SpecWith env
testUnoptimized = do
  it "Produces SQL with superfluous permission checks" \env -> do
    let expected :: Value
        expected =
          [yaml|
           - field: test_schema_foo
             sql: |
                SELECT
                  coalesce(json_agg("root"), '[]') AS "root"
                FROM
                  (
                    SELECT
                      row_to_json(
                        (
                          SELECT
                            "_e"
                          FROM
                            (
                              SELECT
                                "_root.base"."foo_id" AS "foo_id",
                                "_root.ar.root.bars_by_foo_id_to_bar_foo_id"."bars_by_foo_id_to_bar_foo_id" AS "bars_by_foo_id_to_bar_foo_id"
                            ) AS "_e"
                        )
                      ) AS "root"
                    FROM
                      (
                        SELECT
                          *
                        FROM
                          "test_schema"."foo"
                        WHERE
                          (
                            ("test_schema"."foo"."foo_id") <= (('42') :: integer)
                          )
                      ) AS "_root.base"
                      LEFT OUTER JOIN LATERAL (
                        SELECT
                          coalesce(json_agg("bars_by_foo_id_to_bar_foo_id"), '[]') AS "bars_by_foo_id_to_bar_foo_id"
                        FROM
                          (
                            SELECT
                              row_to_json(
                                (
                                  SELECT
                                    "_e"
                                  FROM
                                    (
                                      SELECT
                                        "_root.ar.root.bars_by_foo_id_to_bar_foo_id.base"."bar_id" AS "bar_id"
                                    ) AS "_e"
                                )
                              ) AS "bars_by_foo_id_to_bar_foo_id"
                            FROM
                              (
                                SELECT
                                  *
                                FROM
                                  "test_schema"."bar"
                                WHERE
                                  (
                                    (("_root.base"."foo_id") = ("bar_foo_id"))
                                    AND (
                                      EXISTS (
                                        SELECT
                                          1
                                        FROM
                                          "test_schema"."foo" AS "__be_0_test_schema_foo"
                                        WHERE
                                          (
                                            (
                                              (
                                                ("__be_0_test_schema_foo"."foo_id") = ("test_schema"."bar"."bar_foo_id")
                                              )
                                              AND ('true')
                                            )
                                            AND (
                                              ('true')
                                              AND (
                                                (
                                                  (
                                                    ("__be_0_test_schema_foo"."foo_id") <= (('42') :: integer)
                                                  )
                                                  AND ('true')
                                                )
                                                AND ('true')
                                              )
                                            )
                                          )
                                      )
                                    )
                                  )
                              ) AS "_root.ar.root.bars_by_foo_id_to_bar_foo_id.base"
                          ) AS "_root.ar.root.bars_by_foo_id_to_bar_foo_id"
                      ) AS "_root.ar.root.bars_by_foo_id_to_bar_foo_id" ON ('true')
                  ) AS "_root"
          |]

        schemaName = getter @Schema.SchemaName env

    -- print (getHgeServerInstanceUrl $ getter env)
    -- void getLine

    actual <-
      hgePostExplainRole
        env
        "test-role"
        [graphql|
        query
          {
            #{schemaName}_foo
              {
                foo_id
                bars_by_foo_id_to_bar_foo_id
                {
                  bar_id
                }
              }
          }
        |]

    ignoreWhitespace actual `shouldAtLeastBe` ignoreWhitespace expected
