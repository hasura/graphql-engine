{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that when a table is untracked,  SQL triggers that were created on the
-- tables is also removed
module Test.EventTriggers.PG.EventTriggersUntrackTableCleanupSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (permitTeardownFail)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = const Webhook.runEventsWebhook,
              Fixture.setupTeardown = \(testEnvironment, (webhookServer, _)) ->
                [ permitTeardownFail (Postgres.setupTablesAction (schema "authors") testEnvironment),
                  Fixture.SetupAction
                    { Fixture.setupAction = postgresSetup testEnvironment webhookServer,
                      Fixture.teardownAction = \_ -> pure ()
                    }
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

-- * Backend

-- ** Schema

schema :: Text -> [Schema.Table]
schema authorTableName = [authorsTable authorTableName]

authorsTable :: Text -> Schema.Table
authorsTable tableName =
  (table tableName)
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
-- Tests

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests =
  describe "untrack a table with event triggers should remove the SQL triggers created on the table" do
    it "check: inserting a new row invokes a event trigger"
      $ \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
        let insertQuery =
              [interpolateYaml|
                type: run_sql
                args:
                  source: postgres
                  sql: "INSERT INTO #{schemaName}.authors (id, name) values (3, 'john')"
              |]

            expectedResponse =
              [yaml|
                result_type: CommandOk
                result: null
              |]

            expectedEventPayload =
              [yaml|
                old: null
                new:
                  name: john
                  id: 3
              |]

        -- Insert a row into the table with event trigger
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment insertQuery)
          expectedResponse

        -- Check if there was a payload generated due to the insert statement
        eventPayload <-
          -- wait for the event for a maximum of 5 seconds
          timeout (5 * 1000000) (Chan.readChan eventsQueue)
            >>= (`onNothing` (assertFailure "Event expected, but not fired"))

        eventPayload `shouldBeYaml` expectedEventPayload

    it "untrack table, check the SQL triggers are deleted from the table"
      $ \(testEnvironment, _) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
        let untrackTableQuery =
              [interpolateYaml|
              type: pg_untrack_table
              args:
                source: postgres
                table:
                  schema: #{schemaName}
                  name: authors
            |]

            untrackTableQueryExpectedResponse = [yaml| message: success |]

        -- Untracking the table should remove the SQL triggers on the table
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postMetadata testEnvironment untrackTableQuery)
          untrackTableQueryExpectedResponse

        -- Query the database and see if the trigger exists
        let checkIfTriggerExists =
              [interpolateYaml|
              type: run_sql
              args:
                source: postgres
                sql: "SELECT EXISTS (SELECT 1 FROM pg_trigger WHERE NOT tgisinternal AND tgname = 'notify_#{schemaName}_insert_author_INSERT' AND tgrelid = '#{schemaName}.authors'::regclass);"
            |]

            expectedResponse =
              [yaml|
              result_type: TuplesOk
              result:
                - - exists
                - - f
            |]

        -- If the cleanup happened then the hasura SQL trigger should not exists in the table
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment checkIfTriggerExists)
          expectedResponse

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
    |]
