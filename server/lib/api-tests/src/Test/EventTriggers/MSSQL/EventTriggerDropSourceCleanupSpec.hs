{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that when a source is dropped any event trigger information  such as
-- 'hdb_catalog' schema and the SQL triggers that were created on the tables is also
-- removed
module Test.EventTriggers.MSSQL.EventTriggerDropSourceCleanupSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
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
  describe "dropping a source with event triggers should remove 'hdb_catalog' schema and the SQL triggers created on the table" do
    it "check: inserting a new row invokes a event trigger"
      $ \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
            insertQuery =
              [interpolateYaml|
                type: mssql_run_sql
                args:
                  source: mssql
                  sql: "INSERT INTO #{schemaName}.authors (id, name) values (3, N'john')"
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

    it "drop source, check the table works as it was before event trigger was created on it"
      $ \(testEnvironment, _) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
        let dropSourceQuery =
              [yaml|
              type: mssql_drop_source
              args:
                name: mssql
                cascade: true
            |]

            dropSourceQueryExpectedRespnse = [yaml| message: success |]

        -- Dropping the source should remove 'hdb_catalog' and SQL triggers related to it
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postMetadata testEnvironment dropSourceQuery)
          dropSourceQueryExpectedRespnse

        -- Test that the table works as it was before the event trigger was created
        -- on it. To test that the tables are working, we need to test if
        -- INSERT/DELETE/UPDATE operations on the table works as expected.
        -- We do this in following steps:
        --    1. Reconnect the source, but without event triggers on any tables. We
        --       do this so that we can use run_sql to make SQL queries
        --    2. Do an insert statement and see that it goes through

        let sourceConfig = Sqlserver.defaultSourceConfiguration testEnvironment
            addSourceQuery =
              [yaml|
              type: mssql_add_source
              args:
                name: mssql
                configuration: *sourceConfig
            |]
        _ <- GraphqlEngine.postMetadata testEnvironment addSourceQuery

        let insertQuery =
              [interpolateYaml|
              type: mssql_run_sql
              args:
                source: mssql
                sql: "INSERT INTO #{schemaName}.authors (id, name) values (4, N'harry')"
            |]

            expectedResponse =
              [yaml|
              result_type: CommandOk
              result: null
            |]

        -- If the clean-up did not happen properly this insert query would fail
        -- because the SQL triggers that were created by event triggers might try
        -- to write to a non existent 'hdb_catalog' schema.
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment insertQuery)
          expectedResponse

--------------------------------------------------------------------------------

-- ** Setup and teardown override

mssqlSetupWithEventTriggers :: TestEnvironment -> GraphqlEngine.Server -> IO ()
mssqlSetupWithEventTriggers testEnvironment webhookServer = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
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
          webhook: #{webhookServerEchoEndpoint}
          insert:
            columns: "*"
      |]
