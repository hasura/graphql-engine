{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that the `created_at` value sent in the event payload is
--   correct.
module Test.EventTriggers.PG.EventTriggersCreatedAtUTCSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.Aeson.Internal qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (options))
import Harness.Webhook qualified as Webhook
import Harness.Yaml (fromObject, shouldReturnYaml)
import Hasura.Base.Error (iResultToMaybe)
import Hasura.Prelude
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec (SpecWith, describe, it, shouldSatisfy)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { -- setup the webhook server as the local test environment,
              -- so that the server can be referenced while testing
              Fixture.mkLocalTestEnvironment = const Webhook.run,
              Fixture.setupTeardown = \(testEnvironment, (webhookServer, _)) ->
                [ Postgres.setupTablesAction schema testEnvironment,
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

schema :: [Schema.Table]
schema = [authorsTable]

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

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests =
  describe "created_at captures the correct value of the timestamp at which the event was created at." do
    it "inserting a row in a non UTC timezone shouldn't affect the value of the `created_at`" $
      \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let schemaName :: Schema.SchemaName
            schemaName = Schema.getSchemaName testEnvironment
        let insertQuery =
              [interpolateYaml|
                type: run_sql
                args:
                  source: postgres
                  sql: "SET timezone to 'Asia/Kolkata';INSERT INTO #{schemaName}.authors (id, name) values (3, 'john');"
              |]

            expectedResponse =
              [yaml|
                result_type: CommandOk
                result: null
              |]

        -- Insert a row into the table with event trigger
        shouldReturnYaml
          (options testEnvironment)
          (GraphqlEngine.postV2Query 200 testEnvironment insertQuery)
          expectedResponse

        -- Check if there was a payload generated due to the insert statement
        eventPayload <-
          -- wait for the event for a maximum of 5 seconds
          timeout (5 * 1000000) (Chan.readChan eventsQueue)
            >>= (`onNothing` (assertFailure "Event expected, but not fired"))

        let eventCreatedAtUTCMaybe =
              iResultToMaybe
                =<< Aeson.ifromJSON
                <$> (Key.fromString "created_at")
                `KM.lookup` (fromObject eventPayload)

        eventCreatedAtUTC <-
          onNothing eventCreatedAtUTCMaybe (assertFailure "Error in parsing the `created_at` of the event")

        currentTimeUTC <- liftIO getCurrentTime

        -- The current timestamp in UTC should always be greater than the
        -- event's creation timestamp and also the current time should not
        -- be later than 10 seconds from the event's creation time, since it
        -- is the only event that has to be delivered. By making both the checks,
        -- we can assure that the `created_at_tz` captures the correct value.
        eventCreatedAtUTC
          `shouldSatisfy` (\eventCreatedTime -> eventCreatedTime < currentTimeUTC && addUTCTime 10 eventCreatedTime > currentTimeUTC)

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetup :: TestEnvironment -> GraphqlEngine.Server -> IO ()
postgresSetup testEnvironment webhookServer = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
  let webhookEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/whole_event_payload"
  GraphqlEngine.postMetadata_ testEnvironment $
    [interpolateYaml|
      type: bulk
      args:
      - type: pg_create_event_trigger
        args:
          name: authors_all
          source: postgres
          table:
            name: authors
            schema: #{schemaName}
          webhook: #{webhookEndpoint}
          insert:
            columns: "*"
    |]

postgresTeardown :: TestEnvironment -> IO ()
postgresTeardown testEnvironment = do
  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
      type: bulk
      args:
      - type: pg_delete_event_trigger
        args:
          name: authors_all
          source: postgres
    |]
