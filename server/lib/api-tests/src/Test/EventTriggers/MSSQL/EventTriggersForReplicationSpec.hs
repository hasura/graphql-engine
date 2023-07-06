{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that event triggers are enabled/disabled when logical replication is used
module Test.EventTriggers.MSSQL.EventTriggersForReplicationSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
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
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = const Webhook.runEventsWebhook,
              Fixture.setupTeardown = \(testEnvironment, (_webhookServer, _)) ->
                [ Sqlserver.setupTablesAction (schema "authors" "articles") testEnvironment,
                  Fixture.SetupAction
                    { Fixture.setupAction = pure (),
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
        mssqlSetupWithEventTriggers testEnvironment webhookServer "True"
        let getTriggerInfoQuery =
              [interpolateYaml|
                type: mssql_run_sql
                args:
                  source: mssql
                  sql: "SELECT name, is_not_for_replication FROM sys.triggers WHERE type='TR' ORDER BY name ASC;"
              |]

            expectedResponseForEnablingTriggers =
              [yaml|
                result_type: TuplesOk
                result:
                  -
                    - name
                    - is_not_for_replication
                  -
                    - notify_hasura_author_trigger_DELETE
                    - False
                  -
                    - notify_hasura_author_trigger_INSERT
                    - False
                  -
                    - notify_hasura_author_trigger_UPDATE
                    - False
              |]
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment getTriggerInfoQuery)
          expectedResponseForEnablingTriggers

    it "verify trigger is disabled on logical replication"
      $ \(testEnvironment, (webhookServer, (Webhook.EventsQueue _eventsQueue))) -> do
        mssqlSetupWithEventTriggers testEnvironment webhookServer "False"
        let getTriggerInfoQuery =
              [interpolateYaml|
                type: mssql_run_sql
                args:
                  source: mssql
                  sql: "SELECT name, is_not_for_replication FROM sys.triggers WHERE type='TR' ORDER BY name ASC;"
              |]

            expectedResponseForDisablingTriggers =
              [yaml|
                result_type: TuplesOk
                result:
                  -
                    - name
                    - is_not_for_replication
                  -
                    - notify_hasura_author_trigger_DELETE
                    - True
                  -
                    - notify_hasura_author_trigger_INSERT
                    - True
                  -
                    - notify_hasura_author_trigger_UPDATE
                    - True
              |]
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postV2Query 200 testEnvironment getTriggerInfoQuery)
          expectedResponseForDisablingTriggers

--------------------------------------------------------------------------------

-- ** Setup and teardown override

mssqlSetupWithEventTriggers :: TestEnvironment -> GraphqlEngine.Server -> Text -> IO ()
mssqlSetupWithEventTriggers testEnvironment webhookServer triggerOnReplication = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
  GraphqlEngine.postMetadata_ testEnvironment
    $ [interpolateYaml|
      type: bulk
      args:
      - type: mssql_create_event_trigger
        args:
          name: author_trigger
          source: mssql
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

mssqlTeardown :: TestEnvironment -> IO ()
mssqlTeardown testEnvironment = do
  GraphqlEngine.postMetadata_ testEnvironment
    $ [yaml|
      type: mssql_delete_event_trigger
      args:
        name: author_trigger
        source: mssql
    |]
