{-# LANGUAGE QuasiQuotes #-}

-- | Test if all root fields (list, pk and aggregate) are enabled by default
module Test.DisableRootFields.DefaultRootFieldsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.DisableRootFields.Common
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    ( NE.fromList
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
              setup = sqlServerSetup,
              teardown = SQLServer.teardown schema,
              customOptions = Nothing
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [author]

author :: Schema.Table
author =
  (table "author")
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

--------------------------------------------------------------------------------
-- Setting up postgres

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
    allow_aggregations: true
    columns: '*'
|]

--------------------------------------------------------------------------------
-- Setting up SQL Server

sqlServerSetup :: (TestEnvironment, ()) -> IO ()
sqlServerSetup (testEnvironment, localTestEnvironment) = do
  SQLServer.setup schema (testEnvironment, localTestEnvironment)
  sqlServerCreatePermissions testEnvironment

sqlServerCreatePermissions :: TestEnvironment -> IO ()
sqlServerCreatePermissions testEnvironment = do
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
    allow_aggregations: true
    columns: '*'
|]

--------------------------------------------------------------------------------
-- Tests

-- Root fields are enabled and accessible by default, until specifed otherwise in metadata.
tests :: Context.Options -> SpecWith TestEnvironment
tests opts = describe "DefaultRootFieldSpec" $ do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "1")]
  it "'list' root field is enabled and accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders listQuery)
      listRFEnabledExpectedResponse

  it "'pk' root field is enabled and accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders pkQuery)
      pkRFEnabledExpectedResponse

  it "'aggregate' root field is enabled and accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders aggregateQuery)
      aggRFEnabledExpectedResponse

  it "introspection query: all root fields are enabled and accessible for query" $ \testEnvironment -> do
    let expectedResponse =
          [yaml|
          data:
            __schema:
              queryType:
                fields:
                  - name: hasura_author
                  - name: hasura_author_aggregate
                  - name: hasura_author_by_pk
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders queryTypesIntrospection)
      expectedResponse
