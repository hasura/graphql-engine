{-# LANGUAGE QuasiQuotes #-}

-- | Test if all root fields (list, pk and aggregate) are enabled by default
module Test.Auth.Authorization.DisableRootFields.DefaultRootFieldsSpec (spec) where

import Data.Aeson (Value (String), object, (.=))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (Permission (..), SelectPermissionDetails (..), selectPermission)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (setupPermissionsAction)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
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
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ BigQuery.setupTablesAction schema testEnv,
                  setupPermissionsAction permissions testEnv
                ],
              Fixture.customOptions =
                Just
                  $ Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Citus.setupTablesAction schema testEnv,
                  setupPermissionsAction permissions testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv,
                  setupPermissionsAction permissions testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction schema testEnv,
                  setupPermissionsAction permissions testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ SQLServer.setupTablesAction schema testEnv,
                  setupPermissionsAction permissions testEnv
                ]
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
-- Permissions

permissions :: [Permission]
permissions =
  [ SelectPermission
      selectPermission
        { selectPermissionTable = "author",
          selectPermissionRole = "user",
          selectPermissionColumns = ["id", "name"],
          selectPermissionAllowAggregations = True,
          selectPermissionRows =
            object
              [ "id" .= String "X-Hasura-User-Id"
              ]
        }
  ]

--------------------------------------------------------------------------------
-- Tests

-- Root fields are enabled and accessible by default, until specifed otherwise in metadata.
tests :: SpecWith TestEnvironment
tests = describe "DefaultRootFieldSpec" $ do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "1")]

      backendType :: TestEnvironment -> Maybe Fixture.BackendType
      backendType testEnvironment = fmap Fixture.backendType (getBackendTypeConfig testEnvironment)

  it "'list' root field is enabled and accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (listQuery testEnvironment))
      (listRFEnabledExpectedResponse testEnvironment)

  it "'pk' root field is enabled and accessible" $ \testEnvironment -> do
    -- BigQuery doesn't have primary keys.
    unless (backendType testEnvironment == Just Fixture.BigQuery) do
      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (pkQuery testEnvironment))
        (pkRFEnabledExpectedResponse testEnvironment)

  it "'aggregate' root field is enabled and accessible" $ \testEnvironment -> do
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders (aggregateQuery testEnvironment))
      (aggRFEnabledExpectedResponse testEnvironment)

  it "introspection query: all root fields are enabled and accessible for query" $ \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        expectedResponse :: Value
        expectedResponse = case backendType testEnvironment of
          Just Fixture.BigQuery ->
            [interpolateYaml|
              data:
                __schema:
                  queryType:
                    fields:
                      - name: #{schemaName}_author
                      - name: #{schemaName}_author_aggregate
            |]
          _ ->
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
