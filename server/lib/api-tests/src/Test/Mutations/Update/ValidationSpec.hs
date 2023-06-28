-- |
-- Update objects with input validation
module Test.Mutations.Update.ValidationSpec (spec) where

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
                  createUpdatePermissions testEnvironment server
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runInputValidationWebhook,
              Fixture.setupTeardown = \(testEnvironment, server) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  createUpdatePermissions testEnvironment server
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runInputValidationWebhook,
              Fixture.setupTeardown = \(testEnvironment, server) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  createUpdatePermissions testEnvironment server
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "user")
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
      },
    (table "tweet")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "content" Schema.TStr,
            Schema.column "user_id" Schema.TInt,
            Schema.column "email" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ Schema.reference "user_id" "user" "id"
          ],
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
      }
  ]

createUpdatePermissions :: TestEnvironment -> Server -> Fixture.SetupAction
createUpdatePermissions testEnvironment server =
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
          -- Role user_1 has update permissions tweet table.
          createPermissionMetadata
            testEnvironment
          $ UpdatePermission
            updatePermission
              { updatePermissionTable = "user",
                updatePermissionRole = "user",
                updatePermissionColumns = ["id", "name", "email", "phone_number"],
                updatePermissionValidationWebhook = Just $ (T.pack $ serverUrl server) <> "/validateUpdateUser"
              }

        GraphqlEngine.postMetadata_ testEnvironment
          $
          -- Role user_1 has update permissions tweet table.
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
          -- Role user_1 has update permissions tweet table.
          createPermissionMetadata
            testEnvironment
          $ UpdatePermission
            updatePermission
              { updatePermissionTable = "tweet",
                updatePermissionRole = "user",
                updatePermissionColumns = ["id", "user_id", "content", "email"],
                updatePermissionValidationWebhook = Just $ (T.pack $ serverUrl server) <> "/validateUpdateTweet"
              },
      Fixture.teardownAction = \_ -> pure ()
    }

--------------------------------------------------------------------------------
-- Tests
tests :: SpecWith (TestEnvironment, Server)
tests = describe "Update Input Validation" do
  it "Update a tweet with invalid email" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation update_tweet_invalid_email {
                update_#{schemaName}_tweet(
                  where: { id: {_eq: 1}},
                  _set: {content: "Hi there! Updated", email: "random email"}
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
               message: Invalid email id "random email"
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Update a not-allowed tweet" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation update_user_invalid_email {
                update_#{schemaName}_tweet(
                  where: { id: {_eq: 1}},
                  _set: {content: "Hi there! Updated", email: "test@xyz.com"}
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
               message: Cannot update tweet with id 1
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Update more than the allowed tweets" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation update_user_invalid_email {
                update_#{schemaName}_tweet_many(
                  updates: [
                    { where: { id: {_eq: 1}},
                      _set: {content: "Hi there! Updated"}
                    },
                    { where: { id: {_eq: 2}},
                      _set: {content: "Hi there! Updated"}
                    },
                    { where: { id: {_eq: 3}},
                      _set: {content: "Hi there! Updated"}
                    },
                    { where: { id: {_eq: 4}},
                      _set: {content: "Hi there! Updated"}
                    }
                  ]
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
               message: Cannot update more than 3 tweets at a time
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Update a not-allowed tweet via Primary key" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation update_user_invalid_email {
                update_#{schemaName}_tweet_by_pk(
                  pk_columns: {id: 3},
                  _set: {content: "Hi there! Updated", email: "test@xyz.com"}
                ) {
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
               message: Cannot update tweet with Primary Key (id) 3
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Multiple update in a single mutation: Update tweet with invalid email" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user")]
            [graphql|
              mutation multiple_updates {
                update_#{schemaName}_user(
                    where: { id: {_eq: 4}},
                    _set: {email: "random@bar.com", name: "Foo"}
                  ) {
                    affected_rows
                  }
                
                # Test that the alias works
                update_#{schemaName}_tweet_alias:update_#{schemaName}_tweet(
                  where: { id: {_eq: 4}},
                  _set: {content: "Hi there! Updated", email: "foo@bar.com"}
                ) {
                  affected_rows
                }

                update_#{schemaName}_tweet(
                  where: { id: {_eq: 2}},
                  _set: {content: "Hi there! Updated", email: "random email"}
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
               message: Invalid email id "random email"
          |]

    shouldReturnYaml testEnvironment actual expected
