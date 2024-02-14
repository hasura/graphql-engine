{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Queries.PermissionsKnowAboutColumnNullabilitySpec (spec) where

import Data.Aeson
import Data.Has
import Harness.GlobalTestEnvironment
import Harness.Logging
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Schema qualified as Schema
import Harness.Services.GraphqlEngine
import Harness.Services.Permissions.Table.Postgres
import Harness.Services.Schema
import Harness.Services.Source.Postgres
import Harness.Yaml
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec =
  withHge emptyHgeConfig $ do
    withPostgresSource "postgres-source"
      $ withSchemaName "test_schema"
      $ withPostgresSchema schema
      $ withTablePermissions permissions
      $ test

permissions ::
  ( Has PostgresSource env,
    Has Schema.SchemaName env
  ) =>
  env ->
  [Permission]
permissions env =
  [ SelectPermission
      $ (selectPermission env "foo")
        { selectPermissionColumns = ["nullable_col", "non_nullable_col"],
          selectPermissionRows =
            [yaml| 
              _and:
                - non_nullable_col:
                    _eq: 42
                - nullable_col:
                    _eq: 42
                  |]
        }
  ]

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "foo")
      { Schema.tableColumns =
          [ (Schema.column "nullable_col" Schema.TInt)
              { Schema.columnNullable = True
              },
            (Schema.column "non_nullable_col" Schema.TInt)
              { Schema.columnNullable = False
              }
          ],
        Schema.tableData = []
      }
  ]

--------------------------------------------------------------------------------
-- Tests

test ::
  forall env.
  ( Has HgeServerInstance env,
    Has Logger env,
    Has Schema.SchemaName env
  ) =>
  SpecWith env
test = do
  it "Produces SQL which only check nullable columns for NULL" \env -> do
    let expected :: Value
        expected =
          [interpolateYaml|
           - field: #{schemaName}_foo
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
                              "_root.base"."non_nullable_col" AS "non_nullable_col",
                              "_root.base"."nullable_col" AS "nullable_col"
                          ) AS "_e"
                      )
                    ) AS "root"
                  FROM
                    (
                      SELECT
                        *
                      FROM
                        "#{schemaName}"."foo"
                      WHERE
                        (
                          (
                            ("#{schemaName}"."foo"."non_nullable_col") = (('42') :: integer)
                          )
                          AND (
                            (
                              ("#{schemaName}"."foo"."nullable_col") = (('42') :: integer)
                            )
                            OR (
                              (("#{schemaName}"."foo"."nullable_col") IS NULL)
                              AND ((('42') :: integer) IS NULL)
                            )
                          )
                        )
                    ) AS "_root.base"
                ) AS "_root"
          |]

        schemaName = getter @Schema.SchemaName env

    actual <-
      hgePostExplainRole
        env
        "test-role"
        [graphql|
          query {
            #{schemaName}_foo {
                non_nullable_col
                nullable_col
            }
          }
        |]

    ignoreWhitespace actual `shouldAtLeastBe` ignoreWhitespace expected
