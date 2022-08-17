{-# LANGUAGE QuasiQuotes #-}

-- | Testing regression reported at https://github.com/hasura/graphql-engine/issues/8415
module Test.Regression.DropColumnWithPermissions8415Spec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Harness.Backend.Postgres qualified as Postgres
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  Postgres.setupPermissionsAction [updatePermission] testEnv
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
    { Permissions.permissionTable = authorTableName,
      Permissions.permissionSource = "postgres",
      Permissions.permissionRole = "user",
      Permissions.permissionColumns = ["age"]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe = shouldReturnYaml opts

  it "Drop column which is not referred in update permission" \testEnvironment -> do
    let runSQL = "alter table " <> Constants.postgresDb <> "." <> T.unpack authorTableName <> " drop column name;"

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

    actual `shouldBe` expected

  it "Drop column which is referred in update permission" \testEnvironment -> do
    let runSQL = "alter table " <> Constants.postgresDb <> "." <> T.unpack authorTableName <> " drop column age;"

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
            error: 'cannot drop due to the following dependent objects : permission hasura.author.user.update
              in source "postgres"'
            path: "$"
          |]

    actual `shouldBe` expected
