{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Testing the `run_sql` API
module Test.EventTrigger.EventTriggersRunSQLSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as L8
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http qualified as Http
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (Server (..), TestEnvironment, getServer, stopServer)
import Harness.Webhook qualified as Webhook
import Hasura.Prelude (Text, onLeft, onNothing)
import Network.HTTP.Simple qualified as Http
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec (SpecWith, it, shouldBe)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          -- setup the webhook server as the local test environment,
          -- so that the server can be referenced while testing
          mkLocalTestEnvironment = webhookServerMkLocalTestEnvironment,
          setup = postgresSetup,
          teardown = postgresTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

-- * Backend

-- ** Schema

authorsTable :: Text -> Schema.Table
authorsTable tableName =
  (table tableName)
    { tableColumns =
        [ Schema.column "id" Schema.TStr,
          Schema.column "name" Schema.TStr,
          Schema.column "created_at" Schema.TUTCTime
        ],
      tablePrimaryKey = ["id"]
    }

usersTable :: Schema.Table
usersTable =
  (table "users")
    { tableColumns =
        [ Schema.column "id" Schema.TStr,
          Schema.column "name" Schema.TStr,
          Schema.column "created_at" Schema.TUTCTime
        ],
      tablePrimaryKey = ["id"]
    }

schema :: Text -> [Schema.Table]
schema authorTableName =
  [ authorsTable authorTableName,
    usersTable
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests opts = do
  triggerListeningToAllColumnTests opts
  triggerListeningToSpecificColumnsTests opts
  dropTableContainingTriggerTest opts
  renameTableContainingTriggerTests opts

triggerListeningToAllColumnTests :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
triggerListeningToAllColumnTests opts = do
  it
    ( "when a run_sql query drops a column of a table,"
        <> " it should not throw any error even when an event trigger"
        <> " that accesses all the columns of that table exists"
    )
    $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [yaml|
type: run_sql
args:
  source: postgres
  sql: "ALTER TABLE authors DROP COLUMN created_at;"
|]
        )
        [yaml|
result_type: CommandOk
result: null
         |]
  it "inserting a new row should work fine" $
    \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [yaml|
type: run_sql
args:
  source: postgres
  sql: "INSERT INTO authors (id, name) values (1, 'john') RETURNING name"
|]
        )
        [yaml|
result_type: TuplesOk
result:
  - - name
  - - john
         |]
      eventPayload <-
        -- wait for the event for a maximum of 5 seconds
        timeout (5 * 1000000) (Chan.readChan eventsQueue)
          >>= (`onNothing` (assertFailure "Event expected, but not fired"))
      eventPayload
        `shouldBeYaml` [yaml|
old: null
new:
  name: john
  id: '1'
                                        |]

triggerListeningToSpecificColumnsTests :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
triggerListeningToSpecificColumnsTests _ = do
  it
    ( "when a run_sql query drops a column of a table"
        <> " and an event trigger is defined to access that column"
        <> " dependency error should be thrown"
    )
    $ \((getServer -> Server {urlPrefix, port}), _) -> do
      response <-
        Http.post
          (urlPrefix ++ ":" ++ show port ++ "/v2/query")
          mempty
          [yaml|
type: run_sql
args:
  source: postgres
  sql: "ALTER TABLE users DROP COLUMN created_at;"
|]
      Http.getResponseStatusCode response `shouldBe` 400
      let responseBody = Http.getResponseBody response

      responseValue <-
        eitherDecode responseBody
          `onLeft` \err ->
            assertFailure
              ( "In request: " ++ "/v2/query"
                  ++ "Couldn't decode JSON body:"
                  ++ show err
                  ++ "Body was:"
                  ++ L8.unpack responseBody
              )
      responseValue
        `shouldBeYaml` [yaml|
path: $
error: 'cannot drop due to the following dependent objects : event-trigger hasura.users.users_name_created_at
                   in source "postgres"'
code: dependency-error
                                        |]

dropTableContainingTriggerTest :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
dropTableContainingTriggerTest opts = do
  it
    ( "when a run_sql query drops a table"
        <> " dependency error should be thrown when an event trigger"
        <> " accesses specific columns of the table"
    )
    $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [yaml|
type: run_sql
args:
  source: postgres
  sql: "DROP TABLE users"
|]
        )
        [yaml|
result_type: CommandOk
result: null
         |]

renameTableContainingTriggerTests :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
renameTableContainingTriggerTests opts = do
  it
    ( "when a run_sql query drops a column of a table"
        <> " should not throw any error even when an event trigger"
        <> " that accesses all the columns of that table exists"
    )
    $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [yaml|
type: run_sql
args:
  source: postgres
  sql: "ALTER TABLE authors RENAME TO authors_new;"
|]
        )
        [yaml|
result_type: CommandOk
result: null
         |]
  it "inserting a new row should work fine" $
    \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
      shouldReturnYaml
        opts
        ( GraphqlEngine.postV2Query
            200
            testEnvironment
            [yaml|
type: run_sql
args:
  source: postgres
  sql: "INSERT INTO authors_new (id, name) values (2, 'dan') RETURNING name"
|]
        )
        [yaml|
result_type: TuplesOk
result:
  - - name
  - - dan
         |]
      eventPayload <-
        -- wait for the event for a maximum of 5 seconds
        timeout (5 * 1000000) (Chan.readChan eventsQueue)
          >>= (`onNothing` (assertFailure "Event expected, but not fired"))
      eventPayload
        `shouldBeYaml` [yaml|
old: null
new:
  name: dan
  id: '2'
                                        |]

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
- type: pg_create_event_trigger
  args:
    name: users_name_created_at
    source: postgres
    table:
      name: users
      schema: hasura
    webhook: *webhookServerEchoEndpoint
    insert:
      columns:
        - name
        - created_at
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
  -- only authors table needs to be tear down because
  -- the users table has already been dropped in the
  -- `dropTableContainingTriggerTest` test.

  -- The authors table was renamed in the `renameTableContainingTriggerTests` test
  Postgres.teardown [(authorsTable "authors_new")] (testEnvironment, ())

webhookServerMkLocalTestEnvironment ::
  TestEnvironment -> IO (GraphqlEngine.Server, Webhook.EventsQueue)
webhookServerMkLocalTestEnvironment _ = do
  Webhook.run
