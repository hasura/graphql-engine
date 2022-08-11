{-# LANGUAGE QuasiQuotes #-}

-- | Test if 'list' and 'aggregate' root field is accessible
-- There are 3 different kinds of query root fields and 4 different kinds of
-- subscription root fields. They are mentioned below:
--  * query_root_fields: ["select", "select_by_pk", "select_aggregate"]
--  * subscription_root_fields: ["select", "select_by_pk", "select_aggregate", "select_stream"]
--
-- This test, tests that disabling of 'aggregate' of fields works.
module Test.DisableRootFields.SelectPermission.EnableAggSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
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
spec = do
  let pgFixture =
        (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
          { Fixture.setupTeardown = \(testEnv, _) ->
              [ Postgres.setupTablesAction schema testEnv,
                postgresSetupPermissions testEnv
              ]
          }
      sqlServerFixture =
        (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
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
    commonTests

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
              allow_aggregations: true
              query_root_fields: ["select_aggregate"]
              subscription_root_fields: ["select_aggregate"]
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
-- Setting up SQL server

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
              query_root_fields: ["select_aggregate"]
              subscription_root_fields: ["select_aggregate"]
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

metadataValidationTests :: Fixture.Options -> SpecWith TestEnvironment
metadataValidationTests opts = describe "EnableAggregateRootField Validation test " $ do
  it "the \"select_aggregate\" root field can only be enabled when \"allow_aggregations\" is true" $ \testEnvironment -> do
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
           role: user_with_no_aggregations
           permission:
             filter: {}
             columns: ["name"]
             query_root_fields: ["select", "select_aggregate"]
      |]
      )
      [yaml|
internal:
- definition:
    role: user_with_no_aggregations
    source: postgres
    comment: null
    permission:
      query_root_fields:
      - select_aggregate
      - select
      allow_aggregations: false
      computed_fields: []
      columns:
      - name
      filter: {}
    table:
      schema: hasura
      name: author
  reason: 'Inconsistent object: in table "hasura.author": in permission for role "user_with_no_aggregations":
     The "select_aggregate" root field can only be enabled in the query_root_fields
     or  the subscription_root_fields when "allow_aggregations" is set to true'
  name: select_permission user_with_no_aggregations in table hasura.author in source
     postgres
  type: select_permission
path: $.args
error: 'Inconsistent object: in table "hasura.author": in permission for role "user_with_no_aggregations":
  The "select_aggregate" root field can only be enabled in the query_root_fields or  the
  subscription_root_fields when "allow_aggregations" is set to true'
code: invalid-configuration

      |]

commonTests :: Fixture.Options -> SpecWith TestEnvironment
commonTests opts = describe "EnableAggregateRootField GraphQL common tests" $ do
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

  it "query root: 'aggregate' root field is accessible" $ \testEnvironment -> do
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders aggregateQuery)
      aggRFEnabledExpectedResponse

  it "query_root: introspection query: only 'aggregate' field is accessible" $ \testEnvironment -> do
    let expectedResponse =
          [yaml|
          data:
            __schema:
              queryType:
                fields:
                  - name: hasura_author_aggregate
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders queryTypesIntrospection)
      expectedResponse

  it "subscription_root: introspection query: 'list' and 'aggregate' field is accessible" $ \testEnvironment -> do
    let expectedResponse =
          [yaml|
          data:
            __schema:
              subscriptionType:
                fields:
                  - name: hasura_author_aggregate
          |]

    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders subscriptionTypesIntrospection)
      expectedResponse
