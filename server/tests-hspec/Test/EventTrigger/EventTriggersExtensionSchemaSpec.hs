{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Test that the event triggers works well when the extensions are installed in
-- in a different schema
module Test.EventTrigger.EventTriggersExtensionSchemaSpec (spec) where

import Control.Concurrent.Chan qualified as Chan
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Constants (postgresqlConnectionString)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment, stopServer)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import System.Timeout (timeout)
import Test.HUnit.Base (assertFailure)
import Test.Hspec (SpecWith, describe, it)

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
  checkEventTriggerWhenExtensionInDifferentSchema opts

checkEventTriggerWhenExtensionInDifferentSchema :: Fixture.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
checkEventTriggerWhenExtensionInDifferentSchema opts =
  describe "event triggers should work when extensions are created in different schema using 'extensions_schema'" do
    it "check: inserting a new row invokes a event trigger" $
      \(testEnvironment, (_, (Webhook.EventsQueue eventsQueue))) -> do
        let insertQuery =
              [yaml|
                type: run_sql
                args:
                  source: hge_test
                  sql: "INSERT INTO authors (id, name) values (3, 'john')"
              |]

            expectedResponse =
              [yaml|
                result_type: CommandOk
                result: null
              |]

            expectedEventPayload =
              [yaml|
                old: null
                new:
                  name: john
                  id: 3
              |]

        -- Insert a row into the table with event trigger
        shouldReturnYaml
          opts
          (GraphqlEngine.postV2Query 200 testEnvironment insertQuery)
          expectedResponse

        -- Check if there was a payload generated due to the insert statement
        eventPayload <-
          -- wait for the event for a maximum of 5 seconds
          timeout (5 * 1000000) (Chan.readChan eventsQueue)
            >>= (`onNothing` (assertFailure "Event expected, but not fired"))

        eventPayload `shouldBeYaml` expectedEventPayload

-- TODO: Update the test suite to have a seperate metadata database and source
--       database.
--
-- The following test will not work, because currently the hspec test suite
-- has no seperate metadata DB. This means by the time this test is run, the
-- pgcrypto extension is already used to make the gen_hasura_uuid function.
-- Dropping the extension also will not work, because the 'pgcrypto' extension
-- is only created on fresh database (i.e a database with no hdb_catalog schema).
--
-- Once the test suite has a seperate metadata DB, then we can do the following:
--    1. Drop 'pgcrypto' extension if exists
--    2. Drop 'hdb_catalog' which will only remove event trigger related stuff
--       i.e the hdb_catalog from source db (Which will make the server consider
--       it as fresh DB without any schema present)
--    3. create a new schema for extensions
--    4. Use the schema in the extensions_schema setting of `pg_add_source`
--    5. This will create the pgcrypto extension the new schema
--
-- it "check: pgcrypto is installed in correct extension_schema" $
--   \(testEnvironment, _) -> do
--     let insertQuery =
--           [yaml|
--             type: run_sql
--             args:
--               source: hge_test
--               sql: |
--                 SELECT e.extname AS "Name",n.nspname AS "Schema"
--                 FROM pg_catalog.pg_extension e
--                 LEFT JOIN pg_catalog.pg_namespace n ON n.oid = e.extnamespace
--                 LEFT JOIN pg_catalog.pg_description c ON c.objoid = e.oid AND c.classoid = 'pg_catalog.pg_extension'::pg_catalog.regclass
--                 WHERE e.extname = 'pgcrypto';
--           |]

--         expectedResponse =
--           [yaml|
--             result_type: TuplesOk
--             result:
--               - - Name
--                 - Schema
--               - - pgcrypto
--                 - hge_extension_schema
--           |]

--     -- Insert a row into the table with event trigger
--     shouldReturnYaml
--       opts
--       (GraphqlEngine.postV2Query 200 testEnvironment insertQuery)
--       expectedResponse

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetup :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
postgresSetup (testEnvironment, (webhookServer, _)) = do
  let sourceName = "hge_test"
      extensionsSchema = "hasura" :: Text
      sourceConfig =
        [yaml|
          connection_info:
            database_url: *postgresqlConnectionString
            pool_settings: {}
            extensions_schema: *extensionsSchema
             |]
      webhookServerEchoEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/echo"

  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
      type: pg_add_source
      args:
        name: *sourceName
        configuration: *sourceConfig
    |]

  -- setup tables
  Postgres.createTable testEnvironment (authorsTable "authors")
  Postgres.insertTable (authorsTable "authors")
  Schema.trackTable Fixture.Postgres sourceName (authorsTable "authors") testEnvironment

  -- create the event trigger
  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
      type: bulk
      args:
      - type: pg_create_event_trigger
        args:
          name: authors_all
          source: *sourceName
          table:
            name: authors
            schema: hasura
          webhook: *webhookServerEchoEndpoint
          insert:
            columns: "*"
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
          source: hge_test
    |]
  stopServer server
  Postgres.dropTable (authorsTable "authors")

  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
      type: bulk
      args:
      - type: pg_drop_source
        args:
          name: hge_test
          cascade: true
    |]

webhookServerMkLocalTestEnvironment ::
  TestEnvironment -> IO (GraphqlEngine.Server, Webhook.EventsQueue)
webhookServerMkLocalTestEnvironment _ = do
  Webhook.run
