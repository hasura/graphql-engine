{-# LANGUAGE QuasiQuotes #-}

-- | Test if all root fields are disabled
module Test.Auth.Authorization.DisableRootFields.SelectPermission.DisableAllRootFieldsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
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
              query_root_fields: []
              subscription_root_fields: []
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
-- Tests

tests :: SpecWith TestEnvironment
tests = describe "DisableAllRootFieldsSpec" $ do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "1")]
  it "query root: 'list' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (listQuery testEnvironment))
      (listRFDisabledExpectedResponse testEnvironment)

  it "query root: 'pk' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (pkQuery testEnvironment))
      (pkRFDisabledExpectedResponse testEnvironment)

  it "query root: 'aggregate' root field is disabled and not accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (aggregateQuery testEnvironment))
      (aggRFDisabledExpectedResponse testEnvironment)

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
      testEnvironment
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
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders subscriptionTypesIntrospection)
      expectedResponse
