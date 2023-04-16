{-# LANGUAGE QuasiQuotes #-}

-- | Testing regression reported at https://github.com/hasura/graphql-engine/issues/8415
module Test.Regression.DropColumnWithPermissions8415Spec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions qualified as Permissions
import Harness.Quoter.Yaml (yaml)
import Harness.Schema hiding (runSQL)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (setupPermissionsAction)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  setupPermissionsAction [updatePermission] testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Table]
schema = [author]

authorTableName :: Text
authorTableName = "author"

author :: Table
author =
  (table authorTableName)
    { tableColumns =
        [ column "id" TInt,
          column "name" TStr,
          column "age" TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [VInt 1, VStr "Author 1", VInt 25],
          [VInt 2, VStr "Author 2", VInt 30]
        ]
    }

--------------------------------------------------------------------------------
-- Permission

updatePermission :: Permissions.Permission
updatePermission =
  Permissions.UpdatePermission
    Permissions.updatePermission
      { Permissions.updatePermissionTable = authorTableName,
        Permissions.updatePermissionRole = "user",
        Permissions.updatePermissionColumns = ["age"]
      }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Drop column which is not referred in update permission" \testEnvironment -> do
    let runSQL = "alter table " <> Constants.postgresDb <> "." <> authorTableName <> " drop column name;"

    let actual =
          GraphqlEngine.postV2Query
            200
            testEnvironment
            [yaml|
              type: run_sql
              args:
                source: postgres
                sql: *runSQL
            |]

    let expected =
          [yaml|
            result: null
            result_type: CommandOk
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Drop column which is referred in update permission" \testEnvironment -> do
    let runSQL = "alter table " <> Constants.postgresDb <> "." <> authorTableName <> " drop column age;"

    let actual =
          GraphqlEngine.postV2Query
            400
            testEnvironment
            [yaml|
              type: run_sql
              args:
                source: postgres
                sql: *runSQL
            |]

    let expected =
          [yaml|
            code: dependency-error
            error: 'cannot drop due to the following dependent objects: permission hasura.author.user.update
              in source "postgres"'
            path: "$"
          |]

    shouldReturnYaml testEnvironment actual expected
