{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that event triggers are enabled/disabled when logical replication is used
module Test.EventTriggers.PG.EventTriggersForReplicationSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec

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
              Fixture.setupTeardown = \(testEnvironment, (_webhookServer, _)) ->
                [ Postgres.setupTablesAction (schema "authors" "articles") testEnvironment,
                  Fixture.SetupAction
                    { Fixture.setupAction = pure (),
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
  describe "verify trigger status when logical replication is used" do
    it "verify trigger is enabled on logical replication"
      $ \(testEnvironment, (webhookServer, (Webhook.EventsQueue _eventsQueue))) -> do
        postgresSetupWithEventTriggers testEnvironment webhookServer "True"
        let getTriggerInfoQuery =
              [interpolateYaml|
                type: run_sql
                args:
                  source: postgres
                  sql: "SELECT tgname, tgenabled FROM pg_trigger WHERE tgrelid = 'authors'::regclass ORDER BY tgname ASC;"
              |]

            -- tgenabled: `A` specifies that trigger will always fire, that is, in all modes
            -- origin, local and replica
            expectedResponseForEnablingTriggers =
              [yaml|
                result_type: TuplesOk
                result:
                  -
                    - tgname
                    - tgenabled
                  -
                    - notify_hasura_author_trigger_DELETE
                    - A
                  -
                    - notify_hasura_author_trigger_INSERT
                    - A
                  -
                    - notify_hasura_author_trigger_UPDATE
                    - A
              |]
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment getTriggerInfoQuery)
          expectedResponseForEnablingTriggers

    it "verify trigger is disabled on logical replication"
      $ \(testEnvironment, (webhookServer, (Webhook.EventsQueue _eventsQueue))) -> do
        postgresSetupWithEventTriggers testEnvironment webhookServer "False"
        let getTriggerInfoQuery =
              [interpolateYaml|
                type: run_sql
                args:
                  source: postgres
                  sql: "SELECT tgname, tgenabled FROM pg_trigger WHERE tgrelid = 'authors'::regclass ORDER BY tgname ASC;"
              |]

            -- tgenabled: `O` specifies that trigger will fire in only origin &
            -- local modes, not replica mode
            expectedResponseForDisablingTriggers =
              [yaml|
                result_type: TuplesOk
                result:
                  -
                    - tgname
                    - tgenabled
                  -
                    - notify_hasura_author_trigger_DELETE
                    - O
                  -
                    - notify_hasura_author_trigger_INSERT
                    - O
                  -
                    - notify_hasura_author_trigger_UPDATE
                    - O
              |]
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment getTriggerInfoQuery)
          expectedResponseForDisablingTriggers

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetupWithEventTriggers :: TestEnvironment -> GraphqlEngine.Server -> Text -> IO ()
postgresSetupWithEventTriggers testEnvironment webhookServer triggerOnReplication = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
  GraphqlEngine.postMetadata_ testEnvironment
    $ [interpolateYaml|
      type: pg_create_event_trigger
      args:
        name: author_trigger
        source: postgres
        table:
          name: authors
          schema: #{schemaName}
        webhook: #{webhookServerEchoEndpoint}
        trigger_on_replication: #{triggerOnReplication}
        delete:
          columns: "*"
        insert:
          columns: "*"
        update:
          columns: "*"
      |]

postgresTeardown :: TestEnvironment -> IO ()
postgresTeardown testEnvironment = do
  GraphqlEngine.postMetadata_ testEnvironment
    $ [yaml|
      type: pg_delete_event_trigger
      args:
        name: author_trigger
        source: postgres
    |]
