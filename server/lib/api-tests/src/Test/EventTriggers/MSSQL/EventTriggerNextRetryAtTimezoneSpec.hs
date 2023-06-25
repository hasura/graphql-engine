{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that event trigger retry/deliver
module Test.EventTriggers.MSSQL.EventTriggerNextRetryAtTimezoneSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Control.Concurrent.Extended (sleep)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
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
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = const Webhook.runEventsWebhook,
              Fixture.setupTeardown = \(testEnvironment, (webhookServer, _)) ->
                [ permitTeardownFail (Sqlserver.setupTablesAction (schema "authors") testEnvironment),
                  Fixture.SetupAction
                    { Fixture.setupAction = mssqlSetupWithEventTriggers testEnvironment webhookServer,
                      Fixture.teardownAction = \_ -> mssqlTeardown testEnvironment
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
  describe "event trigger retries if the event trigger is undelivered (and retries are available)" do
    -- The test checks that the event trigger retries as expected. In the test, we fire up the event trigger by adding a
    -- row to the table. We wait for a few seconds so the event has retried completely and then see if the number of
    -- retries are 2 (the event retries once)
    it "check: the total number of tries is (number of retries + 1)"
      $ \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
            insertQuery =
              -- Add row to the table `author` (which will fire up the event trigger)
              [interpolateYaml|
                  type: mssql_run_sql
                  args:
                    source: mssql
                    sql: "INSERT INTO #{schemaName}.authors (id, name) values (3, N'john')"
                |]

            -- get the `tries` column to see if the event was retried
            selectQuery =
              [interpolateYaml|
                      type: mssql_run_sql
                      args:
                        source: mssql
                        sql: "SELECT tries FROM hdb_catalog.event_log"
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

            expectedTotalTries =
              [yaml|
                result_type: TuplesOk
                result: [
                  [tries], [2]
                ]
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

        -- Wait for 15 seconds to make sure that the event trigger has been retried
        sleep $ seconds 10

        -- Check the retries column of hdb_catalog.event_log table to see that the event has been retried once (that is
        -- the event has tried to deliver 2 times in total)
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment selectQuery)
          expectedTotalTries

--------------------------------------------------------------------------------

-- ** Setup and teardown override

mssqlSetupWithEventTriggers :: TestEnvironment -> GraphqlEngine.Server -> IO ()
mssqlSetupWithEventTriggers testEnvironment webhookServer = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
      webhookServerNextRetryEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/nextRetry"
  GraphqlEngine.postMetadata_ testEnvironment
    $ [interpolateYaml|
      type: bulk
      args:
      - type: mssql_create_event_trigger
        args:
          name: authors_all
          source: mssql
          table:
            name: authors
            schema: #{schemaName}
          retry_conf:
            num_retries: 1
            interval_sec: 5
          webhook: #{webhookServerNextRetryEndpoint}
          insert:
            columns: "*"
      |]

mssqlTeardown :: TestEnvironment -> IO ()
mssqlTeardown testEnvironment = do
  GraphqlEngine.postMetadata_ testEnvironment
    $ [yaml|
      type: bulk
      args:
      - type: mssql_delete_event_trigger
        args:
          name: authors_all
          source: mssql
    |]
