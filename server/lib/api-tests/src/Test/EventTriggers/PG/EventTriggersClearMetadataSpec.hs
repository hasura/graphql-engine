{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test event triggers behaviour with the `clear_metadata` API.
module Test.EventTriggers.PG.EventTriggersClearMetadataSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (SetupAction (..), permitTeardownFail)
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
                [ permitTeardownFail (setupTableAction' testEnvironment),
                  Fixture.SetupAction
                    { Fixture.setupAction = postgresSetup testEnvironment webhookServer,
                      Fixture.teardownAction = \_ -> pure ()
                    }
                ]
            }
        ]
    )
    tests
  where
    setupTableAction' testEnvironment =
      -- setup a source named "default" and create and then track the "authors"
      -- table in it.
      SetupAction
        ( do
            GraphqlEngine.setSource testEnvironment (Postgres.defaultNamedSourceMetadata testEnvironment) Nothing
            Postgres.createTable testEnvironment (authorsTable "authors")
            Schema.trackTable "default" (authorsTable "authors") testEnvironment
        )
        (const $ GraphqlEngine.setSources testEnvironment mempty Nothing)

--------------------------------------------------------------------------------

-- * Backend

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
  describe "doing clear_metadata with an event trigger containing auto cleanup config should succeed" do
    it "remove source via replace_metadata, check that the event_log table is removed as well"
      $ \(testEnvironment, (_, _)) -> do
        -- remove the source using replace_meatadata API
        let clearMetadata =
              [yaml|
                type: clear_metadata
                args: {}
              |]

            expectedResponse =
              [yaml|
                message: success
              |]

        -- Checking if the clear_metadata call was successful
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postMetadata testEnvironment clearMetadata)
          expectedResponse

--------------------------------------------------------------------------------

-- ** Setup override

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
          source: default
          table:
            name: authors
            schema: #{schemaName}
          webhook: #{webhookServerEchoEndpoint}
          insert:
            columns: "*"
    |]
