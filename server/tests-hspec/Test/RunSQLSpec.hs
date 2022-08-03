{-# LANGUAGE QuasiQuotes #-}

-- | Test *_run_sql query API
module Test.RunSQLSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Postgres qualified as Postgres
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec

--------------------------------------------------------------------------------
-- Preamble
spec :: SpecWith TestEnvironment
spec = do
  -- BigQuery
  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.BigQuery,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = BigQuery.setup [],
              teardown = BigQuery.teardown [],
              customOptions = Nothing
            }
        ]
    )
    bigqueryTests
  -- Postgres
  Context.run
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.Postgres,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = postgresSetup,
              teardown = postgresTeardown,
              customOptions = Nothing
            }
        ]
    )
    postgresTests

--------------------------------------------------------------------------------
-- Setup & Teardown

postgresSetup :: (TestEnvironment, ()) -> IO ()
postgresSetup (env, ()) = do
  Postgres.setup [testTable] (env, ())
  Postgres.setupPermissions [updatePermission] env

postgresTeardown :: (TestEnvironment, ()) -> IO ()
postgresTeardown (env, ()) = do
  Postgres.teardownPermissions [updatePermission] env
  Postgres.teardown [testTable] (env, ())

updatePermission :: Permissions.Permission
updatePermission =
  Permissions.UpdatePermission
    { Permissions.permissionTable = "test",
      Permissions.permissionSource = "postgres",
      Permissions.permissionRole = "user",
      Permissions.permissionColumns = ["age"]
    }

testTable :: Table
testTable =
  (table "test")
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
-- Tests

bigqueryTests :: Context.Options -> SpecWith TestEnvironment
bigqueryTests opts = do
  it "BigQuery - running invalid SQL" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postV2Query
          400
          testEnvironment
          [yaml|
type: bigquery_run_sql
args:
  source: bigquery
  sql: 'some invalid SQL'
|]
      )
      [yaml|
internal:
  rest_request_non_ok:
    error:
      status: INVALID_ARGUMENT
      code: 400
      message: 'Syntax error: Expected end of input but got keyword SOME at [1:1]'
      errors:
      - location: q
        domain: global
        reason: invalidQuery
        locationType: parameter
        message: 'Syntax error: Expected end of input but got keyword SOME at [1:1]'
path: "$"
error: BigQuery HTTP request failed with status 400 "Bad Request"
code: bigquery-error
|]

postgresTests :: Context.Options -> SpecWith TestEnvironment
postgresTests opts = do
  -- Testing regression reported at https://github.com/hasura/graphql-engine/issues/8415
  it "Drop column which is not referred in update permission" \testEnvironment -> do
    let runSQL = "alter table " <> Constants.postgresDb <> ".test drop column name;"
    shouldReturnYaml
      opts
      ( GraphqlEngine.postV2Query
          200
          testEnvironment
          [yaml|
type: run_sql
args:
  source: postgres
  sql: *runSQL
|]
      )
      [yaml|
result: null
result_type: CommandOk
|]

  it "Drop column which is referred in update permission" \testEnvironment -> do
    let runSQL = "alter table " <> Constants.postgresDb <> ".test drop column age;"
    shouldReturnYaml
      opts
      ( GraphqlEngine.postV2Query
          400
          testEnvironment
          [yaml|
type: run_sql
args:
  source: postgres
  sql: *runSQL
|]
      )
      [yaml|
code: dependency-error
error: 'cannot drop due to the following dependent objects : permission hasura.test.user.update
  in source "postgres"'
path: "$"
|]
