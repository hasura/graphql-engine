{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that when a table is untracked,  SQL triggers that were created on the
-- tables is also removed
module Test.EventTrigger.EventTriggersPGUntrackTableCleanupSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment, stopServer)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.Postgres,
              -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              mkLocalTestEnvironment = webhookServerMkLocalTestEnvironment,
              setup = postgresSetup,
              teardown = postgresTeardown,
              customOptions = Nothing
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

tests :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests opts = do
  cleanupEventTriggersWhenTableUntracked opts

cleanupEventTriggersWhenTableUntracked :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
cleanupEventTriggersWhenTableUntracked opts =
  describe "untrack a table with event triggers should remove the SQL triggers created on the table" do
    it "check: inserting a new row invokes a event trigger" $
      \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let insertQuery =
              [yaml|
                type: run_sql
                args:
                  source: postgres
                  sql: "INSERT INTO authors (id, name) values (3, 'john')"
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

    it "untrack table, check the SQL triggers are deleted from the table" $
      \(testEnvironment, _) -> do
        let untrackTableQuery =
              [yaml|
              type: pg_untrack_table
              args:
                source: postgres
                table: 
                  schema: hasura
                  name: authors
            |]

            untrackTableQueryExpectedResponse = [yaml| message: success |]

        -- Untracking the table should remove the SQL triggers on the table
        shouldReturnYaml
          opts
          (GraphqlEngine.postMetadata testEnvironment untrackTableQuery)
          untrackTableQueryExpectedResponse

        -- Query the database and see if the trigger exists
        let checkIfTriggerExists =
              [yaml|
              type: run_sql
              args:
                source: postgres
                sql: "SELECT EXISTS (SELECT 1 FROM pg_trigger WHERE NOT tgisinternal AND tgname = 'notify_hasura_insert_author_INSERT' AND tgrelid = 'hasura.authors'::regclass);"
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
          opts
          (GraphqlEngine.postV2Query 200 testEnvironment checkIfTriggerExists)
          expectedResponse

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetup :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
postgresSetup (testEnvironment, (webhookServer, _)) = do
  Postgres.setup (schema "authors") (testEnvironment, ())
  let webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
      type: bulk
      args:
      - type: pg_create_event_trigger
        args:
          name: authors_all
          source: postgres
          table:
            name: authors
            schema: hasura
          webhook: *webhookServerEchoEndpoint
          insert:
            columns: "*"
    |]

postgresTeardown :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
postgresTeardown (_, (server, _)) = do
  stopServer server
  Postgres.dropTable (authorsTable "authors")

webhookServerMkLocalTestEnvironment ::
  TestEnvironment -> IO (GraphqlEngine.Server, Webhook.EventsQueue)
webhookServerMkLocalTestEnvironment _ = do
  Webhook.run
