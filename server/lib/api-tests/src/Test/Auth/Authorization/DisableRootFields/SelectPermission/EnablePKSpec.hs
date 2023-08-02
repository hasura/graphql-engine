{-# LANGUAGE QuasiQuotes #-}

-- | Test if only list root field is accessible
module Test.Auth.Authorization.DisableRootFields.SelectPermission.EnablePKSpec (spec) where

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
spec = do
  let pgFixture =
        (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
          { Fixture.setupTeardown = \(testEnv, _) ->
              [ Postgres.setupTablesAction schema testEnv,
                postgresSetupPermissions testEnv
              ]
          }
      sqlServerFixture =
        (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
          { Fixture.setupTeardown = \(testEnv, _) ->
              [ SQLServer.setupTablesAction schema testEnv,
                sqlserverSetupPermissions testEnv
              ]
          }

  Fixture.run
    ( NE.fromList
        [ pgFixture,
          sqlServerFixture
        ]
    )
    graphQLTests

  Fixture.run
    ( NE.fromList
        [ pgFixture
        ]
    )
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
              query_root_fields: ["select_by_pk"]
              subscription_root_fields: ["select_by_pk"]
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
              query_root_fields: ["select_by_pk"]
              subscription_root_fields: ["select_by_pk"]
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

metadataValidationTests :: SpecWith TestEnvironment
metadataValidationTests = describe "EnablePKRootField Validation test " $ do
  it "a role not having access to the primary key column(s) should not be allowed to enable the primary key root field" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postMetadataWithStatus
          400
          testEnvironment
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
      - select
      - select_by_pk
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

graphQLTests :: SpecWith TestEnvironment
graphQLTests = describe "EnablePKRootFieldSpec GraphQL tests" $ do
  let userHeaders = [("x-hasura-role", "user"), ("x-hasura-user-id", "1")]
  it "query root: 'list' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (listQuery testEnvironment))
      (listRFDisabledExpectedResponse testEnvironment)

  it "query root: 'pk' root field is accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (pkQuery testEnvironment))
      (pkRFEnabledExpectedResponse testEnvironment)

  it "query root: 'aggregate' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (aggregateQuery testEnvironment))
      (aggRFDisabledExpectedResponse testEnvironment)

  it "query_root: introspection query: only  'pk' field is accessible" $ \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expectedResponse =
          [interpolateYaml|
            data:
              __schema:
                queryType:
                  fields:
                    - name: #{schemaName}_author_by_pk
          |]

    shouldReturnYaml
      testEnvironment
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
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders subscriptionTypesIntrospection)
      expectedResponse
