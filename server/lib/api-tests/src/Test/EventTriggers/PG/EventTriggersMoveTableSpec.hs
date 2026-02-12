{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that when an event trigger is moved from one table to another via
--   `replace_metadata`, the SQL triggers are properly cleaned up from the old
--   table and created on the new table.
module Test.EventTriggers.PG.EventTriggersMoveTableSpec (spec) where

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
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
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
                [ permitTeardownFail (Postgres.setupTablesAction schema testEnvironment),
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

-- We need two tables to move the trigger between
authorsTable :: Schema.Table
authorsTable =
  (table "authors")
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

articlesTable :: Schema.Table
articlesTable =
  (table "articles")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Article 1"]
        ]
    }

schema :: [Schema.Table]
schema = [authorsTable, articlesTable]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests =
  describe "moving an event trigger between tables via replace_metadata should clean up SQL triggers from the old table" do
    it "after moving trigger from authors to articles, SQL trigger should not exist on authors table"
      $ \(testEnvironment, (webhookServer, _)) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
            webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"

        -- First verify the trigger exists on authors table
        -- Trigger name format is: notify_hasura_<triggerName>_<OPERATION>
        -- Query uses tgrelid to filter by table
        let checkTriggerOnAuthors =
              [interpolateYaml|
                type: run_sql
                args:
                  source: postgres
                  sql: SELECT EXISTS (SELECT 1 FROM pg_trigger t JOIN pg_class c ON t.tgrelid = c.oid JOIN pg_namespace n ON c.relnamespace = n.oid WHERE NOT t.tgisinternal AND t.tgname = 'notify_hasura_my_trigger_INSERT' AND c.relname = 'authors' AND n.nspname = '#{schemaName}');
              |]

            triggerExistsResponse =
              [yaml|
                result_type: TuplesOk
                result:
                  - - exists
                  - - t
              |]

        -- Trigger should exist on authors before the move
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment checkTriggerOnAuthors)
          triggerExistsResponse

        -- Now move the trigger to articles table using replace_metadata
        let sourceConfig = Postgres.defaultSourceConfiguration testEnvironment
            replaceMetadataMoveTrigger =
              [interpolateYaml|
                type: replace_metadata
                args:
                  version: 3
                  sources:
                  - name: postgres
                    kind: postgres
                    configuration: #{sourceConfig}
                    tables:
                    - table:
                        name: authors
                        schema: #{schemaName}
                    - table:
                        name: articles
                        schema: #{schemaName}
                      event_triggers:
                      - name: my_trigger
                        definition:
                          enable_manual: true
                          insert:
                            columns: "*"
                        retry_conf:
                          interval_sec: 10
                          num_retries: 0
                          timeout_sec: 60
                        webhook: #{webhookServerEchoEndpoint}
              |]

            expectedReplaceResponse =
              [yaml|
                message: success
              |]

        -- Execute the replace_metadata to move the trigger
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postMetadata testEnvironment replaceMetadataMoveTrigger)
          expectedReplaceResponse

        -- Reload metadata to ensure SQL triggers are created/cleaned up
        -- This is necessary because replace_metadata doesn't immediately create SQL triggers
        let reloadMetadata =
              [yaml|
                type: reload_metadata
                args:
                  recreate_event_triggers: true
              |]

        _ <- GraphqlEngine.postMetadata testEnvironment reloadMetadata

        -- Now verify the trigger NO LONGER exists on authors table
        let triggerNotExistsResponse =
              [yaml|
                result_type: TuplesOk
                result:
                  - - exists
                  - - f
              |]

        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment checkTriggerOnAuthors)
          triggerNotExistsResponse

        -- And verify the trigger NOW exists on articles table (same trigger name)
        let checkTriggerOnArticles =
              [interpolateYaml|
                type: run_sql
                args:
                  source: postgres
                  sql: SELECT EXISTS (SELECT 1 FROM pg_trigger t JOIN pg_class c ON t.tgrelid = c.oid JOIN pg_namespace n ON c.relnamespace = n.oid WHERE NOT t.tgisinternal AND t.tgname = 'notify_hasura_my_trigger_INSERT' AND c.relname = 'articles' AND n.nspname = '#{schemaName}');
              |]

        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment checkTriggerOnArticles)
          triggerExistsResponse

--------------------------------------------------------------------------------

-- ** Setup and teardown

postgresSetup :: TestEnvironment -> GraphqlEngine.Server -> IO ()
postgresSetup testEnvironment webhookServer = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
  -- Create an event trigger on the authors table initially
  GraphqlEngine.postMetadata_ testEnvironment
    $ [interpolateYaml|
      type: bulk
      args:
      - type: pg_create_event_trigger
        args:
          name: my_trigger
          source: postgres
          table:
            name: authors
            schema: #{schemaName}
          webhook: #{webhookServerEchoEndpoint}
          insert:
            columns: "*"
    |]

postgresTeardown :: TestEnvironment -> IO ()
postgresTeardown _testEnvironment = do
  -- Teardown is handled by permitTeardownFail wrapping the setupTablesAction
  -- The event trigger will be cleaned up when the source is dropped
  pure ()
