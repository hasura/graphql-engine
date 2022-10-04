{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that when a source is removed via `replace_metadata` API, the cleanup
--   is done properly.
module Test.EventTrigger.PG.EventTriggersReplaceMetadataCleanupSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.String (fromString)
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Backend.Postgres qualified as Postgres
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment, stopServer)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, shouldBe)

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

tests :: Fixture.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests opts = do
  cleanupEventTriggersWhenSourceRemoved opts

cleanupEventTriggersWhenSourceRemoved :: Fixture.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
cleanupEventTriggersWhenSourceRemoved opts =
  describe "removing a source with event trigger via replace_metadata should also remove the event trigger related stuffs (hdb_catalog.event_log)" do
    it "remove source via replace_metadata, check that the event_log table is removed as well" $
      \(testEnvironment, (_, _)) -> do
        -- `hdb_catalog.event_log` should be existing before (as we have added an event trigger in setup)
        checkIfPGTableExists "hdb_catalog.event_log" >>= (`shouldBe` True)

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
          opts
          (GraphqlEngine.postMetadata testEnvironment replaceMetadata)
          expectedResponse

        -- `hdb_catalog.event_log` should not be existing now
        checkIfPGTableExists "hdb_catalog.event_log" >>= (`shouldBe` False)

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
  Postgres.dropTable (authorsTable "authors")
  stopServer server

webhookServerMkLocalTestEnvironment ::
  TestEnvironment -> IO (GraphqlEngine.Server, Webhook.EventsQueue)
webhookServerMkLocalTestEnvironment _ = do
  Webhook.run

checkIfPGTableExists :: String -> IO Bool
checkIfPGTableExists tableName = do
  let sqlQuery = "SELECT to_regclass('" <> tableName <> "')::text;"
      handleNullExp (Postgres.UnexpectedNull {}) = pure False
      handleNullExp err = throw err
  catch
    ( bracket
        (Postgres.connectPostgreSQL (fromString Constants.postgresqlConnectionString))
        Postgres.close
        ( \conn -> do
            rows :: [[String]] <- Postgres.query_ conn (fromString sqlQuery)
            pure (((head . head) rows) == tableName)
        )
    )
    handleNullExp
