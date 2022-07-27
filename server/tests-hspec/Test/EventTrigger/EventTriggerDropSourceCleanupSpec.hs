{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that when a source is dropped any event trigger information  such as
-- 'hdb_catalog' schema and the SQL triggers that were created on the tables is also
-- removed
module Test.EventTrigger.EventTriggerDropSourceCleanupSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment, stopServer)
import Harness.Webhook qualified as Webhook
import Hasura.Prelude
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    [ Context.Context
        { name = Context.Backend Context.SQLServer,
          -- setup the webhook server as the local test environment,
          -- so that the server can be referenced while testing
          mkLocalTestEnvironment = webhookServerMkLocalTestEnvironment,
          setup = mssqlSetupWithEventTriggers,
          teardown = mssqlTeardown,
          customOptions = Nothing
        }
    ]
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

tests :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests opts = do
  cleanupEventTriggersWhenSourceDropped opts

cleanupEventTriggersWhenSourceDropped :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
cleanupEventTriggersWhenSourceDropped opts =
  describe "dropping a source with event triggers should remove 'hdb_catalog' schema and the SQL triggers created on the table" do
    it "check: inserting a new row invokes a event trigger" $
      \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let insertQuery =
              [yaml|
                type: mssql_run_sql
                args:
                  source: mssql
                  sql: "INSERT INTO authors (id, name) values (3, N'john')"
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
          opts
          (GraphqlEngine.postV2Query 200 testEnvironment insertQuery)
          expectedResponse

        -- Check if there was a payload generated due to the insert statement
        eventPayload <-
          -- wait for the event for a maximum of 5 seconds
          timeout (5 * 1000000) (Chan.readChan eventsQueue)
            >>= (`onNothing` (assertFailure "Event expected, but not fired"))

        eventPayload `shouldBeYaml` expectedEventPayload

    it "drop source, check the table works as it was before event trigger was created on it" $
      \(testEnvironment, _) -> do
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
          opts
          (GraphqlEngine.postMetadata testEnvironment dropSourceQuery)
          dropSourceQueryExpectedRespnse

        -- Test that the table works as it was before the event trigger was created
        -- on it. To test that the tables are working, we need to test if
        -- INSERT/DELETE/UPDATE operations on the table works as expected.
        -- We do this in following steps:
        --    1. Reconnect the source, but without event triggers on any tables. We
        --       do this so that we can use run_sql to make SQL queries
        --    2. Do an insert statement and see that it goes through

        let sourceConfig = Sqlserver.defaultSourceConfiguration
            addSourceQuery =
              [yaml|
              type: mssql_add_source
              args:
                name: mssql
                configuration: *sourceConfig
            |]
        _ <- GraphqlEngine.postMetadata testEnvironment addSourceQuery

        let insertQuery =
              [yaml|
              type: mssql_run_sql
              args:
                source: mssql
                sql: "INSERT INTO authors (id, name) values (4, N'harry')"
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
          opts
          (GraphqlEngine.postV2Query 200 testEnvironment insertQuery)
          expectedResponse

--------------------------------------------------------------------------------

-- ** Setup and teardown override

mssqlSetupWithEventTriggers :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
mssqlSetupWithEventTriggers (testEnvironment, (webhookServer, _)) = do
  Sqlserver.setup (schema "authors") (testEnvironment, ())
  let webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
      type: bulk
      args:
      - type: mssql_create_event_trigger
        args:
          name: authors_all
          source: mssql
          table:
            name: authors
            schema: hasura
          webhook: *webhookServerEchoEndpoint
          insert:
            columns: "*"
      |]

mssqlTeardown :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
mssqlTeardown (_, (server, _)) = do
  stopServer server
  Sqlserver.dropTable (authorsTable "authors")

webhookServerMkLocalTestEnvironment ::
  TestEnvironment -> IO (GraphqlEngine.Server, Webhook.EventsQueue)
webhookServerMkLocalTestEnvironment _ = do
  Webhook.run
