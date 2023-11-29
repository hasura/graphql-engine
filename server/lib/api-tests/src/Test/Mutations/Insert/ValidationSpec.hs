-- |
-- Insert objects with input validation
module Test.Mutations.Insert.ValidationSpec (spec) where

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
                  createInsertPermissions testEnvironment server
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runInputValidationWebhook,
              Fixture.setupTeardown = \(testEnvironment, server) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  createInsertPermissions testEnvironment server
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runInputValidationWebhook,
              Fixture.setupTeardown = \(testEnvironment, server) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  createInsertPermissions testEnvironment server
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
        tablePrimaryKey = ["id"]
      },
    (table "tweet")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "content" Schema.TStr,
            Schema.column "user_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ Schema.reference "user_id" "user" "id"
          ]
      }
  ]

createInsertPermissions :: TestEnvironment -> Server -> Fixture.SetupAction
createInsertPermissions testEnvironment server =
  Fixture.SetupAction
    { Fixture.setupAction = do
        GraphqlEngine.postMetadata_ testEnvironment
          $
          -- Role user_1 has insert permissions user table.
          createPermissionMetadata
            testEnvironment
          $ InsertPermission
            insertPermission
              { insertPermissionTable = "user",
                insertPermissionRole = "user_1",
                insertPermissionColumns = (["id", "name", "email", "phone_number"] :: [Text]),
                insertPermissionValidationWebhook = Just $ (T.pack $ serverUrl server) <> "/validateUser"
              }

        GraphqlEngine.postMetadata_ testEnvironment
          $
          -- Role user_1 has insert permissions tweet table.
          createPermissionMetadata
            testEnvironment
          $ InsertPermission
            insertPermission
              { insertPermissionTable = "tweet",
                insertPermissionRole = "user_1",
                insertPermissionColumns = (["id", "user_id", "content"] :: [Text]),
                insertPermissionValidationWebhook = Just $ (T.pack $ serverUrl server) <> "/validateTweet"
              },
      Fixture.teardownAction = \_ -> pure ()
    }

--------------------------------------------------------------------------------
-- Tests
tests :: SpecWith (TestEnvironment, Server)
tests = describe "Insert Input Validation" do
  it "Adds a user with invalid email" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment
        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user_1")]
            [graphql|
              mutation insert_user_invalid_email {
                insert_#{schemaName}_user(
                  objects: [{
                    name: "user_1",
                    email: "random email"
                    phone_number: "9876543210"
                  }]
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

  it "Adds a user with invalid phone number" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user_1")]
            [graphql|
              mutation insert_user_invalid_email {
                insert_#{schemaName}_user(
                  objects: [{
                    name: "user_1",
                    email: "random@email.com"
                    phone_number: "987654321"
                  }]
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
               message: Invalid phone number "987654321"
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Adds a user with more than one tweets" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user_1")]
            [graphql|
              mutation insert_user_more_tweets {
                insert_#{schemaName}_user(
                  objects: [{
                    name: "user_1",
                    email: "random@email.com"
                    phone_number: "9876543210"
                    tweets_by_id_to_user_id: {
                      data: [
                      {content: "Hi there!"},
                      {content: "Hi there Again!"}
                      ]
                    }
                  }]
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
               message: Only one tweet is allowed to be added with a user
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Insert a tweet with more exceeded content length" \(testEnvironment, _server) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          GraphqlEngine.postGraphqlWithHeaders
            testEnvironment
            [("X-Hasura-Role", "user_1")]
            [graphql|
              mutation insert_tweet_more_content {
                insert_#{schemaName}_tweet(
                  objects: [{
                    content: "Hi there, this is first tweet inserting into Hasura with more content than expected, validation should fail"
                    user_by_user_id_to_id: {
                      data: {
                        name: "user_1",
                        email: "random@email.com",
                        phone_number: "9087654321"
                      }
                    }
                  }]
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
               message: Tweet should not contain more than 30 characters
          |]

    shouldReturnYaml testEnvironment actual expected
