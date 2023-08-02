{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that when a source is removed via `replace_metadata` API, the cleanup
--   is done properly.
module Test.EventTriggers.PG.EventTriggersReplaceMetadataCleanupSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.String (fromString)
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Backend.Postgres qualified as Postgres
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Services.Database.Postgres qualified as Postgres
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (permitTeardownFail)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, shouldBe)

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
                [ permitTeardownFail (Postgres.setupTablesAction (schema "authors") testEnvironment),
                  Fixture.SetupAction
                    { Fixture.setupAction = postgresSetup testEnvironment webhookServer,
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

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests =
  describe "removing a source with event trigger via replace_metadata should also remove the event trigger related stuffs (hdb_catalog.event_log)" do
    it "remove source via replace_metadata, check that the event_log table is removed as well"
      $ \(testEnvironment, (_, _)) -> do
        -- `hdb_catalog.event_log` should be existing before (as we have added an event trigger in setup)
        checkIfPGTableExists testEnvironment "hdb_catalog.event_log" >>= (`shouldBe` True)

        -- remove the source using replace_meatadata API
        let replaceMetadata =
              [yaml|
                type: replace_metadata
                args:
                  sources: []
                  version : 3
              |]

            expectedResponse =
              [yaml|
                message: success
              |]

        -- Checking if the replace_metadata was successful
        shouldReturnYaml
          testEnvironment
          (GraphqlEngine.postMetadata testEnvironment replaceMetadata)
          expectedResponse

        -- `hdb_catalog.event_log` should not be existing now
        checkIfPGTableExists testEnvironment "hdb_catalog.event_log" >>= (`shouldBe` False)

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

checkIfPGTableExists :: TestEnvironment -> String -> IO Bool
checkIfPGTableExists testEnvironment tableName = do
  let sqlQuery = "SELECT to_regclass('" <> tableName <> "')::text;"
      handleNullExp (Postgres.UnexpectedNull {}) = pure False
      handleNullExp err = throw err
  catch
    ( bracket
        (Postgres.connectPostgreSQL (txtToBs $ Postgres.getPostgresServerUrl $ Postgres.makeFreshDbConnectionString testEnvironment))
        Postgres.close
        ( \conn -> do
            rows :: [[String]] <- Postgres.query_ conn (fromString sqlQuery)
            pure (((head . head) rows) == tableName)
        )
    )
    handleNullExp
