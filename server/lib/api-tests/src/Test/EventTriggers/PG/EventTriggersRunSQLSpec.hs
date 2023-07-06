{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Testing the `run_sql` API
module Test.EventTriggers.PG.EventTriggersRunSQLSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http qualified as Http
import Harness.Quoter.Yaml
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (permitTeardownFail)
import Harness.TestEnvironment (GlobalTestEnvironment, Server (..), TestEnvironment, getServer)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Network.HTTP.Simple qualified as Http
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec (SpecWith, it, shouldBe)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironmentSingleSetup
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = const Webhook.runEventsWebhook,
              Fixture.setupTeardown = \(testEnvironment, (webhookServer, _)) ->
                [ permitTeardownFail (Postgres.setupTablesAction (schema "authors") testEnvironment),
                  Fixture.SetupAction
                    { Fixture.setupAction = postgresSetup testEnvironment webhookServer,
                      Fixture.teardownAction = \_ -> postgresTeardown testEnvironment
                    }
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

-- * Backend

-- ** Schema

authorsTable :: Text -> Schema.Table
authorsTable tableName =
  (table tableName)
    { tableColumns =
        [ Schema.column "id" Schema.TStr,
          Schema.column "name" Schema.TStr,
          Schema.column "created_at" Schema.TUTCTime
        ],
      tablePrimaryKey = ["id"]
    }

usersTable :: Schema.Table
usersTable =
  (table "users")
    { tableColumns =
        [ Schema.column "id" Schema.TStr,
          Schema.column "name" Schema.TStr,
          Schema.column "created_at" Schema.TUTCTime
        ],
      tablePrimaryKey = ["id"]
    }

schema :: Text -> [Schema.Table]
schema authorTableName =
  [ authorsTable authorTableName,
    usersTable
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests = do
  triggerListeningToAllColumnTests
  triggerListeningToSpecificColumnsTests
  dropTableContainingTriggerTest
  renameTableContainingTriggerTests

triggerListeningToAllColumnTests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
triggerListeningToAllColumnTests = do
  it
    ( "when a run_sql query drops a column of a table,"
        <> " it should not throw any error even when an event trigger"
        <> " that accesses all the columns of that table exists"
    )
    $ \(testEnvironment, _) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [interpolateYaml|
type: run_sql
args:
  source: postgres
  sql: "ALTER TABLE #{schemaName}.authors DROP COLUMN created_at;"
|]
        )
        [yaml|
result_type: CommandOk
result: null
         |]
  it "inserting a new row should work fine"
    $ \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [interpolateYaml|
              type: run_sql
              args:
                source: postgres
                sql: "INSERT INTO #{schemaName}.authors (id, name) values (1, 'john') RETURNING name"
            |]
        )
        [yaml|
result_type: TuplesOk
result:
  - - name
  - - john
         |]
      eventPayload <-
        -- wait for the event for a maximum of 5 seconds
        timeout (5 * 1000000) (Chan.readChan eventsQueue)
          >>= (`onNothing` (assertFailure "Event expected, but not fired"))
      eventPayload
        `shouldBeYaml` [yaml|
old: null
new:
  name: john
  id: '1'
                                        |]

triggerListeningToSpecificColumnsTests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
triggerListeningToSpecificColumnsTests = do
  -- TODO: Use postWithHeadersStatus to match the errors instead of the call via getResponseBody
  it
    ( "when a run_sql query drops a column of a table"
        <> " and an event trigger is defined to access that column"
        <> " dependency error should be thrown"
    )
    $ \(testEnvironment, _) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
          Server {urlPrefix, port} = getServer testEnvironment
      response <-
        Http.post
          (urlPrefix ++ ":" ++ show port ++ "/v2/query")
          mempty
          [interpolateYaml|
type: run_sql
args:
  source: postgres
  sql: "ALTER TABLE #{schemaName}.users DROP COLUMN created_at;"
|]
      Http.getResponseStatusCode response `shouldBe` 400
      let responseBody = Http.getResponseBody response

      responseValue <-
        eitherDecode responseBody
          `onLeft` \err ->
            assertFailure
              ( "In request: "
                  ++ "/v2/query"
                  ++ "Couldn't decode JSON body:"
                  ++ show err
                  ++ "Body was:"
                  ++ L8.unpack responseBody
              )
      responseValue
        `shouldBeYaml` [interpolateYaml|
path: $
error: 'cannot drop due to the following dependent objects: event-trigger #{schemaName}.users.users_name_created_at
                   in source "postgres"'
code: dependency-error
                                        |]

dropTableContainingTriggerTest :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
dropTableContainingTriggerTest = do
  it
    ( "when a run_sql query drops a table"
        <> " dependency error should be thrown when an event trigger"
        <> " accesses specific columns of the table"
    )
    $ \(testEnvironment, _) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [interpolateYaml|
              type: run_sql
              args:
                source: postgres
                sql: "DROP TABLE #{schemaName}.users"
            |]
        )
        [yaml|
result_type: CommandOk
result: null
         |]

renameTableContainingTriggerTests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
renameTableContainingTriggerTests = do
  it
    ( "when a run_sql query drops a column of a table"
        <> " should not throw any error even when an event trigger"
        <> " that accesses all the columns of that table exists"
    )
    $ \(testEnvironment, _) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [interpolateYaml|
              type: run_sql
              args:
                source: postgres
                sql: "ALTER TABLE #{schemaName}.authors RENAME TO authors_new;"
              |]
        )
        [yaml|
           result_type: CommandOk
           result: null
         |]
  it "inserting a new row should work fine"
    $ \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [interpolateYaml|
              type: run_sql
              args:
                source: postgres
                sql: "INSERT INTO #{schemaName}.authors_new (id, name) values (2, 'dan') RETURNING name"
            |]
        )
        [yaml|
          result_type: TuplesOk
          result:
            - - name
            - - dan
        |]
      eventPayload <-
        -- wait for the event for a maximum of 5 seconds
        timeout (5 * 1000000) (Chan.readChan eventsQueue)
          >>= (`onNothing` (assertFailure "Event expected, but not fired"))
      eventPayload
        `shouldBeYaml` [yaml|
old: null
new:
  name: dan
  id: '2'
                                        |]

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetup :: TestEnvironment -> GraphqlEngine.Server -> IO ()
postgresSetup testEnvironment webhookServer = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
  let webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
  GraphqlEngine.postMetadata_ testEnvironment
    $ [interpolateYaml|
      type: bulk
      args:
      - type: pg_create_event_trigger
        args:
          name: authors_all
          source: postgres
          table:
            name: authors
            schema: #{schemaName}
          webhook: #{webhookServerEchoEndpoint}
          insert:
            columns: "*"
      - type: pg_create_event_trigger
        args:
          name: users_name_created_at
          source: postgres
          table:
            name: users
            schema: #{schemaName}
          webhook: #{webhookServerEchoEndpoint}
          insert:
            columns:
              - name
              - created_at
    |]

postgresTeardown :: TestEnvironment -> IO ()
postgresTeardown testEnvironment = do
  GraphqlEngine.postMetadata_ testEnvironment
    $ [yaml|
      type: bulk
      args:
      - type: pg_delete_event_trigger
        args:
          name: authors_all
          source: postgres
    |]
  -- only authors table needs to be tear down because
  -- the users table has already been dropped in the
  -- `dropTableContainingTriggerTest` test.

  -- The authors table was renamed in the `renameTableContainingTriggerTests` test
  Postgres.dropTableIfExists testEnvironment (authorsTable "authors_new")
