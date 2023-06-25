{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that only event triggers with unique names are allowed
module Test.EventTriggers.PG.EventTriggersUniqueNameSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.Aeson (Value (..))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
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
                [ Postgres.setupTablesAction (schema "authors" "articles") testEnvironment,
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

schema :: Text -> Text -> [Schema.Table]
schema authorTableName articleTableName = [authorsTable authorTableName, articlesTable articleTableName]

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

articlesTable :: Text -> Schema.Table
articlesTable tableName =
  (table tableName)
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Article 1"],
          [Schema.VInt 2, Schema.VStr "Article 2"]
        ]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests =
  describe "only unique trigger names are allowed" do
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

    it "metadata_api: does not allow creating an event trigger with a name that already exists"
      $ \(testEnvironment, (webhookServer, _)) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
        -- metadata <- GraphqlEngine.exportMetadata testEnvironment
        let webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
        let createEventTriggerWithDuplicateName =
              [interpolateYaml|
              type: pg_create_event_trigger
              args:
                name: authors_all
                source: postgres
                table:
                    name: articles
                    schema: #{schemaName}
                webhook: #{webhookServerEchoEndpoint}
                insert:
                    columns: "*"
            |]

            createEventTriggerWithDuplicateNameExpectedResponse =
              [yaml|
                code: already-exists
                error: Event trigger with name "authors_all" already exists
                path: $.args
              |]

        -- Creating a event trigger with duplicate name should fail
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postMetadataWithStatus 400 testEnvironment createEventTriggerWithDuplicateName)
          createEventTriggerWithDuplicateNameExpectedResponse

    it "replace_metadata: does not allow creating an event trigger with a name that already exists"
      $ \(testEnvironment, (webhookServer, _)) -> do
        let replaceMetadata = getReplaceMetadata testEnvironment webhookServer

            replaceMetadataWithDuplicateNameExpectedResponse =
              [yaml|
                code: not-supported
                error: 'Event trigger with duplicate names not allowed: "authors_all"'
                path: $.args
              |]

        -- Creating a event trigger with duplicate name should fail
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postMetadataWithStatus 400 testEnvironment replaceMetadata)
          replaceMetadataWithDuplicateNameExpectedResponse

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

getReplaceMetadata :: TestEnvironment -> GraphqlEngine.Server -> Value
getReplaceMetadata testEnvironment webhookServer =
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
   in [interpolateYaml|
      type: replace_metadata
      args:
        version: 3
        sources:
        - configuration: #{sourceConfig}
          name: postgres
          kind: postgres
          tables:
          - table:
              schema: #{schemaName}
              name: authors
            event_triggers:
            - name: authors_all
              definition:
                enable_manual: true
                insert:
                  columns: "*"
              retry_conf:
                interval_sec: 10
                num_retries: 0
                timeout_sec: 60
              webhook: #{webhookServerEchoEndpoint}
          - table:
              schema: #{schemaName}
              name: articles
            event_triggers:
            - name: authors_all
              definition:
                enable_manual: true
                insert:
                  columns: "*"
                update:
                  columns: "*"
              retry_conf:
                interval_sec: 10
                num_retries: 0
                timeout_sec: 60
              webhook: #{webhookServerEchoEndpoint}
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
