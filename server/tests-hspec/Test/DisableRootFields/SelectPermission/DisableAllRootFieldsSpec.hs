{-# LANGUAGE QuasiQuotes #-}

-- | Test if all root fields are disabled
module Test.DisableRootFields.SelectPermission.DisableAllRootFieldsSpec (spec) where

import Harness.Backend.Postgres qualified as Postgres
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
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = postgresSetup,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        }
    ]
    tests

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
    query_root_fields: []
    subscription_root_fields: []
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = describe "DisableAllRootFieldsSpec" $ do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "1")]
  it "query root: 'list' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders listQuery)
      listRFDisabledExpectedResponse

  it "query root: 'pk' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders pkQuery)
      pkRFDisabledExpectedResponse

  it "query root: 'aggregate' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders aggregateQuery)
      aggRFDisabledExpectedResponse

  it "query_root: introspection query: all root fields are disabled and not accessible" $ \testEnvironment -> do
    let expectedResponse =
          [yaml|
          data:
            __schema:
              queryType:
                fields:
                  - name: no_queries_available
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders queryTypesIntrospection)
      expectedResponse

  it "subscription_root: introspection query: all root fields disabled and not accessible" $ \testEnvironment -> do
    let expectedResponse =
          [yaml|
          data:
            __schema:
              subscriptionType: null
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders subscriptionTypesIntrospection)
      expectedResponse
