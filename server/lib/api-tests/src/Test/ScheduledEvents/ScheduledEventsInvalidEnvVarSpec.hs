{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that scheduled triggers with invalid/missing env vars adds (errored) invocation logs
module Test.ScheduledEvents.ScheduledEventsInvalidEnvVarSpec (spec) where

import Control.Concurrent.Extended (sleep)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
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
tests = do
  scheduledEventsWithInvalidEnvVar

scheduledEventsWithInvalidEnvVar :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
scheduledEventsWithInvalidEnvVar =
  describe "creating a scheduled event with invalid env var should add a failed invocation log" do
    it "check the invocation log requests added for failed request corresponding to invalid header"
      $ \(testEnvironment, (_, _)) -> do
        -- get all the scheduled event invocations
        let getScheduledEventInvocationsQuery =
              [yaml|
              type: get_scheduled_event_invocations
              args:
                type: one_off
            |]

        -- wait for all scheduled event retries to complete
        sleep $ seconds 20

        apiRes <- GraphqlEngine.postMetadata testEnvironment getScheduledEventInvocationsQuery

        -- get the number of invocations present
        let resArray = KM.lookup "invocations" $ fromObject apiRes
            resLength = case resArray of
              Just (Array arr) -> length arr
              _ -> 0

        -- a scheduled event with 1 retry (and invalid header) should add 2 invocation logs
        resLength `shouldBe` 2

--------------------------------------------------------------------------------

-- ** Setup override

postgresSetup :: TestEnvironment -> GraphqlEngine.Server -> IO ()
postgresSetup testEnvironment webhookServer = do
  let webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"
  GraphqlEngine.postMetadata_ testEnvironment
    $ [interpolateYaml|
      type: create_scheduled_event
      args:
        webhook: #{webhookServerEchoEndpoint}
        schedule_at: '2023-03-06T13:36:00Z'
        payload:
          test: 1
        headers:
          - name: test_header
            value_from_env: invalid_envvar3
        retry_conf:
          num_retries: 1
          retry_interval_seconds: 5
    |]

fromObject :: Value -> Object
fromObject (Object x) = x
fromObject v = error $ "fromObject: Expected object, received" <> show v
