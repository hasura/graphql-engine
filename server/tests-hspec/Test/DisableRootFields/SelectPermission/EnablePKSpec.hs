{-# LANGUAGE QuasiQuotes #-}

-- | Test if only list root field is accessible
module Test.DisableRootFields.SelectPermission.EnablePKSpec (spec) where

import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.DisableRootFields.Common
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = do
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = postgresSetup,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = mssqlSetup,
          teardown = SQLServer.teardown schema,
          customOptions = Nothing
        }
    ]
    graphQLTests
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        }
    ]
    metadataValidationTests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "author")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Author 1"],
            [Schema.VInt 2, Schema.VStr "Author 2"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Setting up Postgres

postgresSetup :: (TestEnvironment, ()) -> IO ()
postgresSetup (testEnvironment, localTestEnvironment) = do
  Postgres.setup schema (testEnvironment, localTestEnvironment)
  postgresCreatePermissions testEnvironment

postgresCreatePermissions :: TestEnvironment -> IO ()
postgresCreatePermissions testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_create_select_permission
args:
  source: postgres
  table:
    schema: hasura
    name: author
  role: user
  permission:
    filter:
      id: X-Hasura-User-Id
    columns: '*'
    query_root_fields: ["select_by_pk"]
    subscription_root_fields: ["select_by_pk"]
|]

--------------------------------------------------------------------------------
-- Setting up SQL Server

mssqlSetup :: (TestEnvironment, ()) -> IO ()
mssqlSetup (testEnvironment, localTestEnvironment) = do
  SQLServer.setup schema (testEnvironment, localTestEnvironment)
  mssqlCreatePermissions testEnvironment

mssqlCreatePermissions :: TestEnvironment -> IO ()
mssqlCreatePermissions testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: mssql_create_select_permission
args:
  source: mssql
  table:
    schema: hasura
    name: author
  role: user
  permission:
    filter:
      id: X-Hasura-User-Id
    columns: '*'
    query_root_fields: ["select_by_pk"]
    subscription_root_fields: ["select_by_pk"]
|]

--------------------------------------------------------------------------------
-- Tests

metadataValidationTests :: Context.Options -> SpecWith TestEnvironment
metadataValidationTests opts = describe "EnablePKRootField Validation test " $ do
  it "a role not having access to the primary key column(s) should not be allowed to enable the primary key root field" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      ( GraphqlEngine.postWithHeadersStatus
          400
          testEnvironment
          "/v1/metadata"
          mempty
          [yaml|
            type: pg_create_select_permission
            args:
              source: postgres
              table:
                schema: hasura
                name: author
              role: user_with_no_access_to_PK
              permission:
                filter: {}
                columns: ["name"]
                query_root_fields: ["select", "select_by_pk"]
         |]
      )
      [yaml|
internal:
- definition:
    role: user_with_no_access_to_PK
    source: postgres
    comment:
    permission:
      query_root_fields:
      - select_by_pk
      - select
      allow_aggregations: false
      computed_fields: []
      columns:
      - name
      filter: {}
    table:
      schema: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in permission for role "user_with_no_access_to_PK":
    The "select_by_pk" field cannot be included in the query_root_fields or subscription_root_fields
    because the role "user_with_no_access_to_PK" does not have access to the primary
    key of the table "hasura.author" in the source "postgres"'
  name: select_permission user_with_no_access_to_PK in table hasura.author in source
    postgres
  type: select_permission
path: "$.args"
error: 'Inconsistent object: in table "hasura.author": in permission for role "user_with_no_access_to_PK":
  The "select_by_pk" field cannot be included in the query_root_fields or subscription_root_fields
  because the role "user_with_no_access_to_PK" does not have access to the primary
  key of the table "hasura.author" in the source "postgres"'
code: invalid-configuration

      |]

graphQLTests :: Context.Options -> SpecWith TestEnvironment
graphQLTests opts = describe "EnablePKRootFieldSpec GraphQL tests" $ do
  let userHeaders = [("x-hasura-role", "user"), ("x-hasura-user-id", "1")]
  it "query root: 'list' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders listQuery)
      listRFDisabledExpectedResponse

  it "query root: 'pk' root field is accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders pkQuery)
      pkRFEnabledExpectedResponse

  it "query root: 'aggregate' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders aggregateQuery)
      aggRFDisabledExpectedResponse

  it "query_root: introspection query: only  'pk' field is accessible" $ \testEnvironment -> do
    let expectedResponse =
          [yaml|
          data:
            __schema:
              queryType:
                fields:
                  - name: hasura_author_by_pk
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders queryTypesIntrospection)
      expectedResponse

  it "subscription_root: introspection query: only 'pk' field is accessible" $ \testEnvironment -> do
    let expectedResponse =
          [yaml|
          data:
            __schema:
              subscriptionType:
                fields:
                  - name: hasura_author_by_pk
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders subscriptionTypesIntrospection)
      expectedResponse
