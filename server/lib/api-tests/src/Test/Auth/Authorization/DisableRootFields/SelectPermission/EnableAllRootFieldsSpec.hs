{-# LANGUAGE QuasiQuotes #-}

-- | Test if all root fields are accessible
module Test.Auth.Authorization.DisableRootFields.SelectPermission.EnableAllRootFieldsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Auth.Authorization.DisableRootFields.Common
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  postgresSetupPermissions testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ SQLServer.setupTablesAction schema testEnv,
                  sqlserverSetupPermissions testEnv
                ]
            }
        ]
    )
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

postgresSetupPermissions :: TestEnvironment -> Fixture.SetupAction
postgresSetupPermissions testEnv =
  Fixture.SetupAction
    { setupAction =
        GraphqlEngine.postMetadata_
          testEnv
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
              allow_aggregations: true
              query_root_fields: ["select", "select_by_pk", "select_aggregate"]
              subscription_root_fields: ["select", "select_by_pk", "select_aggregate", "select_stream"]
          |],
      teardownAction = \_ ->
        GraphqlEngine.postMetadata_
          testEnv
          [yaml|
          type: pg_drop_select_permission
          args:
            source: postgres
            table:
              schema: hasura
              name: author
            role: user
          |]
    }

--------------------------------------------------------------------------------
-- Setting up SQL Server

sqlserverSetupPermissions :: TestEnvironment -> Fixture.SetupAction
sqlserverSetupPermissions testEnv =
  Fixture.SetupAction
    { setupAction =
        GraphqlEngine.postMetadata_
          testEnv
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
              allow_aggregations: true
              query_root_fields: ["select", "select_by_pk", "select_aggregate"]
              subscription_root_fields: ["select", "select_by_pk", "select_aggregate", "select_stream"]
          |],
      teardownAction = \_ ->
        GraphqlEngine.postMetadata_
          testEnv
          [yaml|
          type: mssql_drop_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: author
            role: user
          |]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = describe "EnableAllRootFieldsSpec" $ do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "1")]
  it "query root: 'list' root field is accessible" $ \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (listQuery testEnvironment))
      [interpolateYaml|
        data:
          #{schemaName}_author:
            - id: 1
              name: Author 1
      |]

  it "query root: 'pk' root field is accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (pkQuery testEnvironment))
      (pkRFEnabledExpectedResponse testEnvironment)

  it "query root: 'aggregate' root field is accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (aggregateQuery testEnvironment))
      (aggRFEnabledExpectedResponse testEnvironment)

  it "query_root: introspection query: all root fields are accessible" $ \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expectedResponse =
          [interpolateYaml|
            data:
              __schema:
                queryType:
                  fields:
                    - name: #{schemaName}_author
                    - name: #{schemaName}_author_aggregate
                    - name: #{schemaName}_author_by_pk
          |]

    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders queryTypesIntrospection)
      expectedResponse
