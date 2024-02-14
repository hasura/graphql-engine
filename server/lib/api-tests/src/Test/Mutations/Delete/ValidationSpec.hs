-- |
-- Delete objects with input validation
module Test.Mutations.Delete.ValidationSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment, serverUrl)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runInputValidationWebhook,
              Fixture.setupTeardown = \(testEnvironment, server) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  createDeletePermissions testEnvironment server
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runInputValidationWebhook,
              Fixture.setupTeardown = \(testEnvironment, server) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  createDeletePermissions testEnvironment server
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runInputValidationWebhook,
              Fixture.setupTeardown = \(testEnvironment, server) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  createDeletePermissions testEnvironment server
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "tweet")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "content" Schema.TStr,
            Schema.column "user_id" Schema.TInt,
            Schema.column "email" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Hi there!",
              Schema.VInt 1,
              Schema.VStr "test@b.com"
            ],
            [ Schema.VInt 2,
              Schema.VStr "Hi there Again!",
              Schema.VInt 2,
              Schema.VStr "test@c.com"
            ],
            [ Schema.VInt 3,
              Schema.VStr "Hi there Again!",
              Schema.VInt 3,
              Schema.VStr "test@d.com"
            ],
            [ Schema.VInt 4,
              Schema.VStr "Hi there Again!",
              Schema.VInt 4,
              Schema.VStr "test@e.com"
            ]
          ]
      },
    (table "user")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr,
            Schema.column "email" Schema.TStr,
            Schema.column "phone_number" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Foo",
              Schema.VStr "test@b.com",
              Schema.VStr "9999999999"
            ],
            [ Schema.VInt 2,
              Schema.VStr "Bar",
              Schema.VStr "test@c.com",
              Schema.VStr "9999999999"
            ],
            [ Schema.VInt 3,
              Schema.VStr "Hi there Again!",
              Schema.VStr "test@d.com",
              Schema.VStr "8888888888"
            ],
            [ Schema.VInt 4,
              Schema.VStr "Hi there Again!",
              Schema.VStr "test@e.com",
              Schema.VStr "7777777777"
            ]
          ]
      }
  ]

createDeletePermissions :: TestEnvironment -> Server -> Fixture.SetupAction
createDeletePermissions testEnvironment server =
  Fixture.SetupAction
    { Fixture.setupAction = do
        GraphqlEngine.postMetadata_ testEnvironment
          $
          -- Role user_1 has update permissions tweet table.
          createPermissionMetadata
            testEnvironment
          $ SelectPermission
            selectPermission
              { selectPermissionTable = "user",
                selectPermissionRole = "user",
                selectPermissionColumns = ["id", "name", "email", "phone_number"]
              }

        GraphqlEngine.postMetadata_ testEnvironment
          $
          -- Role user_1 has delete permissions on tweet table.
          createPermissionMetadata
            testEnvironment
          $ DeletePermission
            deletePermission
              { deletePermissionTable = "user",
                deletePermissionRole = "user",
                deletePermissionValidationWebhook = Just $ T.pack (serverUrl server) <> "/validateDeleteMutation"
              }

        GraphqlEngine.postMetadata_ testEnvironment
          $
          -- Role user_1 has select permissions on tweet table.
          createPermissionMetadata
            testEnvironment
          $ SelectPermission
            selectPermission
              { selectPermissionTable = "tweet",
                selectPermissionRole = "user",
                selectPermissionColumns = ["id", "user_id", "content", "email"]
              }

        GraphqlEngine.postMetadata_ testEnvironment
          $
          -- Role user_1 has delete permissions on tweet table.
          createPermissionMetadata
            testEnvironment
          $ DeletePermission
            deletePermission
              { deletePermissionTable = "tweet",
                deletePermissionRole = "user",
                deletePermissionValidationWebhook = Just $ T.pack (serverUrl server) <> "/validateDeleteMutation"
              },
      Fixture.teardownAction = \_ -> pure ()
    }

--------------------------------------------------------------------------------
-- Tests
tests :: SpecWith (TestEnvironment, Server)
tests = describe "Delete Input Validation" do
  it "Delete a not allowed tweet" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation delete_user_invalid_email {
                delete_#{schemaName}_tweet(
                  where: { id: {_eq: 1}}
                ) {
                  affected_rows
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            errors:
             - extensions:
                 code: validation-failed
                 path: $
               message: Cannot delete tweet with id 1
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Delete a not-allowed tweet via Primary key" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation delete_user_invalid_email {
                delete_#{schemaName}_tweet_by_pk(id: 3) {
                  id,
                  email
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            errors:
             - extensions:
                 code: validation-failed
                 path: $
               message: Cannot delete tweet with Primary Key (id) 3
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Multiple delete in a single mutation: delete tweet with invalid email" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation multiple_updates {
                delete_#{schemaName}_user(
                  where: { id: {_eq: 2}}
                ) {
                  affected_rows
                }
                
                # Test that the alias works
                delete_#{schemaName}_tweet_alias:delete_#{schemaName}_tweet(
                  where: { id: {_eq: 3}}
                ) {
                  affected_rows
                }

                delete_#{schemaName}_tweet(
                  where: { id: {_eq: 1}}
                ) {
                  affected_rows
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            errors:
             - extensions:
                 code: validation-failed
                 path: $
               message: Cannot delete tweet with id 1
          |]

    shouldReturnYaml testEnvironment actual expected
