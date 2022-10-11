{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that only event triggers with unique names are allowed
module Test.EventTrigger.PG.EventTriggersUniqueNameSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.Aeson (Value (..))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.BackendType
import Harness.Test.Fixture qualified as Fixture
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
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = webhookServerMkLocalTestEnvironment,
              Fixture.setupTeardown = \testEnv ->
                [ Fixture.SetupAction
                    { Fixture.setupAction = postgresSetup testEnv,
                      Fixture.teardownAction = \_ -> postgresTeardown testEnv
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

tests :: Fixture.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests opts = do
  duplicateTriggerNameNotAllowed opts

duplicateTriggerNameNotAllowed :: Fixture.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
duplicateTriggerNameNotAllowed opts =
  describe "only unique trigger names are allowed" do
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

    it "metadata_api: does not allow creating an event trigger with a name that already exists" $
      \(testEnvironment, (webhookServer, _)) -> do
        -- metadata <- GraphqlEngine.exportMetadata testEnvironment
        let webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
        let createEventTriggerWithDuplicateName =
              [yaml|
              type: pg_create_event_trigger
              args:
                name: authors_all
                source: postgres
                table:
                    name: articles
                    schema: hasura
                webhook: *webhookServerEchoEndpoint
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
          opts
          (GraphqlEngine.postWithHeadersStatus 400 testEnvironment "/v1/metadata/" mempty createEventTriggerWithDuplicateName)
          createEventTriggerWithDuplicateNameExpectedResponse

    it "replace_metadata: does not allow creating an event trigger with a name that already exists" $
      \(testEnvironment, (webhookServer, _)) -> do
        let replaceMetadata = getReplaceMetadata webhookServer

            replaceMetadataWithDuplicateNameExpectedResponse =
              [yaml| 
                code: not-supported
                error: 'Event trigger with duplicate names not allowed: "authors_all"'
                path: $.args
              |]

        -- Creating a event trigger with duplicate name should fail
        shouldReturnYaml
          opts
          (GraphqlEngine.postWithHeadersStatus 400 testEnvironment "/v1/metadata/" mempty replaceMetadata)
          replaceMetadataWithDuplicateNameExpectedResponse

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetup :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
postgresSetup (testEnvironment, (webhookServer, _)) = do
  Postgres.setup (schema "authors" "articles") (testEnvironment, ())
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

getReplaceMetadata :: GraphqlEngine.Server -> Value
getReplaceMetadata webhookServer =
  let sourceConfig = Postgres.defaultSourceConfiguration
      schemaName = schemaKeyword Fixture.Postgres
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
   in [yaml|
      type: replace_metadata
      args: 
        version: 3
        sources:
        - configuration: *sourceConfig
          name: postgres
          kind: postgres
          tables:
          - table:
              schema: *schemaName
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
              webhook: *webhookServerEchoEndpoint
          - table:
              schema: *schemaName
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
              webhook: *webhookServerEchoEndpoint
    |]

postgresTeardown :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
postgresTeardown (testEnvironment, (server, _)) = do
  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
      type: bulk
      args:
      - type: pg_delete_event_trigger
        args:
          name: authors_all
          source: postgres
    |]
  stopServer server
  Postgres.dropTable (authorsTable "authors")
  Postgres.dropTable (articlesTable "articles")

webhookServerMkLocalTestEnvironment ::
  TestEnvironment -> IO (GraphqlEngine.Server, Webhook.EventsQueue)
webhookServerMkLocalTestEnvironment _ = do
  Webhook.run
