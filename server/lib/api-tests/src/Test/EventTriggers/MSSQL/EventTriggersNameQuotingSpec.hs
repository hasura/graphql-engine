{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that event trigger names are quoted properly
module Test.EventTriggers.MSSQL.EventTriggersNameQuotingSpec (spec) where

import Data.Aeson (Value)
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
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------

-- ** Spec

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = const Webhook.runEventsWebhook,
              Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction (schema "authors") testEnvironment,
                  Fixture.SetupAction
                    { Fixture.setupAction = pure (),
                      Fixture.teardownAction = \_ -> pure ()
                    }
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

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

-- ** Tests

-- | Weird (non-alphanumeric/underscore/hyphen) trigger names used to be allowed, only
-- producing a warning -- there's no documented reason that permissiveness was
-- desirable, and it turned out to be the root of a SQL injection vulnerability
-- (the name is interpolated into generated trigger SQL), so it's now a hard error.
tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests = describe "weird trigger names are disallowed" do
  it "metadata_api: disallow creating an event trigger with weird name via replace_metadata"
    $ \(testEnvironment, (webhookServer, _)) -> do
      let createEventTriggerWithWeirdName =
            addEventTriggerViaReplaceMetadata testEnvironment "weird]name]" webhookServer
          createEventTriggerWithWeirdNameExpectedResponse =
            [yaml|
              code: not-supported
              error: Starting in v2.50 only alphanumeric and underscore and hyphens allowed for name
              path: $.args
            |]

      -- Creating a event trigger with weird name should fail
      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postMetadataWithStatus 400 testEnvironment createEventTriggerWithWeirdName)
        createEventTriggerWithWeirdNameExpectedResponse

      let checkAllSQLTriggersQuery =
            [yaml|
            type: mssql_run_sql
            args:
              source: mssql
              sql: |-
                SELECT tr.name AS TriggerName FROM sys.triggers tr
                INNER JOIN sys.tables t ON t.object_id = tr.parent_id
                WHERE t.name in ('authors');
          |]

      -- Check that no trigger was created on the database. Note: unlike a query
      -- that returns rows (TuplesOk), a SELECT matching zero rows is reported by
      -- the MSSQL ODBC driver as CommandOk with a null result.
      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postV2Query 200 testEnvironment checkAllSQLTriggersQuery)
        [yaml|
          result_type: CommandOk
          result: null
        |]

--------------------------------------------------------------------------------

-- ** Helper

addEventTriggerViaReplaceMetadata :: TestEnvironment -> Text -> GraphqlEngine.Server -> Value
addEventTriggerViaReplaceMetadata testEnvironment eventTriggerName webhookServer =
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
      sourceConfig = Sqlserver.defaultSourceConfiguration testEnvironment
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
   in [interpolateYaml|
      type: replace_metadata
      args:
        version: 3
        sources:
        - configuration: #{sourceConfig}
          name: mssql
          kind: mssql
          tables:
          - table:
              schema: #{schemaName}
              name: authors
            event_triggers:
            - name: #{eventTriggerName}
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
