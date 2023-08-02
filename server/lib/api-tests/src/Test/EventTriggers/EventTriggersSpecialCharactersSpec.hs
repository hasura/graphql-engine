{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that special characters are handled correctly in event trigger payload
module Test.EventTriggers.EventTriggersSpecialCharactersSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (permitTeardownFail)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
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
                [ permitTeardownFail (Sqlserver.setupTablesAction schema testEnvironment),
                  Fixture.SetupAction
                    { Fixture.setupAction = dbSetup testEnvironment webhookServer,
                      Fixture.teardownAction = \_ -> pure ()
                    }
                ]
            },
          (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = const Webhook.runEventsWebhook,
              Fixture.setupTeardown = \(testEnvironment, (webhookServer, _)) ->
                [ permitTeardownFail (Postgres.setupTablesAction schema testEnvironment),
                  Fixture.SetupAction
                    { Fixture.setupAction = dbSetup testEnvironment webhookServer,
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

-- | Create a dummy table since the creation of schema is now moved into 'create_table'
-- function. Hence having a table is necessary to create the schema for the test.
dummyTable :: Schema.Table
dummyTable =
  (Schema.table "dummy_table")
    { Schema.tableColumns =
        [Schema.column "dummy_column" Schema.TStr],
      Schema.tablePrimaryKey = ["dummy_column"]
    }

schema :: [Schema.Table]
schema = [dummyTable]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests =
  describe "special characters of different languages in event trigger payload are encoded in UTF-8" do
    it "check: inserting a new row invokes a event trigger"
      $ \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let backendTypeMetadata = fromMaybe (error "Expected a backend type but got nothing") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata
            schemaName = Schema.getSchemaName testEnvironment
            -- TODO: backendPrefixRunSql will no longer be needed, once
            -- https://github.com/hasura/graphql-engine-mono/pull/7465 is merged.
            backendPrefixRunSql =
              case BackendType.backendTypeString backendTypeMetadata of
                "pg" -> ""
                x -> x <> "_"

            insertQuery =
              [interpolateYaml|
                type: #{backendPrefixRunSql}run_sql
                args:
                  source: #{sourceName}
                  sql: "INSERT INTO #{schemaName}.authors (id, specialøñámé) values (3, 'john')"
              |]

            expectedResponse =
              [yaml|
                result_type: CommandOk
                result: null
              |]

            -- The column name which had special language characters is displayed
            -- correctly in the payload
            expectedEventPayload =
              [yaml|
                old: null
                new:
                  specialøñámé: john
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

--------------------------------------------------------------------------------

-- ** Setup and teardown override

dbSetup :: TestEnvironment -> GraphqlEngine.Server -> IO ()
dbSetup testEnvironment webhookServer = do
  let backendTypeMetadata = fromMaybe (error "Expected a backend type but got nothing") $ getBackendTypeConfig testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
      sourceName = BackendType.backendSourceName backendTypeMetadata
      backendPrefix = BackendType.backendTypeString backendTypeMetadata
      -- TODO: backendPrefixRunSql will no longer be needed, once
      -- https://github.com/hasura/graphql-engine-mono/pull/7465 is merged.
      backendPrefixRunSql =
        case BackendType.backendTypeString backendTypeMetadata of
          "pg" -> ""
          x -> x <> "_"
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"

  -- Create table via run_sql
  GraphqlEngine.postV2Query_
    testEnvironment
    [interpolateYaml|
      type: #{backendPrefixRunSql}run_sql
      args:
        source: #{sourceName}
        sql: |
          CREATE TABLE #{schemaName}.authors (
            id INT PRIMARY KEY,
            specialøñámé VARCHAR(255)
          );
    |]

  -- Track table using custom_name for the special character column since GraphQL
  -- spec does not support special characters
  GraphqlEngine.postMetadata_ testEnvironment
    $ [interpolateYaml|
      type: bulk
      args:
      - type: #{backendPrefix}_track_table
        args:
          source: #{sourceName}
          table:
            schema: #{schemaName}
            name: authors
          configuration:
            column_config:
              specialøñámé:
                custom_name: special_

      - type: #{backendPrefix}_create_event_trigger
        args:
          name: authors_all
          source: #{sourceName}
          table:
            name: authors
            schema: #{schemaName}
          webhook: #{webhookServerEchoEndpoint}
          insert:
            columns: "*"
    |]
