{-# LANGUAGE QuasiQuotes #-}

module Test.EventTrigger.EventTriggersRecreationSpec (spec) where

import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postV2Query_)
import Harness.GraphqlEngine qualified as GraphQLEngine
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment, stopServer)
import Harness.Webhook qualified as Webhook
import Test.Hspec (SpecWith, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    [ Context.Context
        { name = Context.Backend Context.Postgres,
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

schema :: [Schema.Table]
schema = [usersTable]

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetup :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
postgresSetup (testEnvironment, _) = do
  -- In the setup, we create postgres's event triggers that capture every DDL
  -- change made in the database and then store them in a table called
  -- `ddl_history` that contains metadata about the DDL query like
  -- the query that was executed, time at which the query was executed,
  -- what type of query it was etc.
  Postgres.setup schema (testEnvironment, ())
  GraphqlEngine.postV2Query_
    testEnvironment
    [yaml|
       type: run_sql
       args:
         source: postgres
         sql: |

           DROP TABLE IF EXISTS hdb_catalog.hdb_source_catalog_version;

           DROP INDEX IF EXISTS hdb_catalog.hdb_source_catalog_version_one_row;

           DROP TABLE IF EXISTS hdb_catalog.event_invocation_logs CASCADE;

           DROP TABLE IF EXISTS hdb_catalog.event_log;

           DROP INDEX IF EXISTS hdb_catalog.event_log_trigger_name;

           DROP TABLE IF EXISTS hdb_catalog.event_invocation_logs_event_id;

           CREATE TABLE hasura.ddl_history (
             id serial primary key,
             event text,
             tag text,
             object_type text,
             schema_name text,
             object_identity text,
             query text,
             created_at timestamptz default now()
           );

           CREATE or REPLACE FUNCTION hasura.log_ddl_command()
             RETURNS event_trigger AS
             $$ DECLARE
             v1 text;
             r record;
           BEGIN
           select
             query into v1
           from
             pg_stat_activity
           where
             pid = pg_backend_pid();
           -- RAISE NOTICE 'ddl event:%, command:%', tg_event, tg_tag;
           -- NB:since ddl_command_end cannot collect the details of the drop statement, we use sql_drop
           if TG_EVENT = 'ddl_command_end' then
           SELECT
             * into r
           FROM
             pg_event_trigger_ddl_commands();
             if r.classid > 0
             then
               insert into hasura.ddl_history(
             event, tag, object_type, schema_name, object_identity, query
           )
           values
             (
               TG_EVENT, TG_TAG, r.object_type, r.schema_name,
               r.object_identity, v1
             );
           end if;
           end if;
           if TG_EVENT = 'sql_drop' then -- To avoid repeated collection, we filtered 'ALTER TABLE' and 'ALTER FOREIGN TABLE'
           if TG_TAG != 'ALTER TABLE'
           and TG_TAG != 'ALTER FOREIGN TABLE' then
           SELECT
             * into r
           FROM
             pg_event_trigger_dropped_objects();
           insert into postgres.ddl_history(
             event, tag,  object_type, schema_name, object_identity, query
           )
           values
             (
               TG_EVENT, TG_TAG, r.object_type, r.schema_name,
               r.object_identity, v1
             );
           end if;
           end if;
           end;
           $$ LANGUAGE plpgsql;

           CREATE EVENT TRIGGER pg_get_ddl_command on ddl_command_end EXECUTE PROCEDURE hasura.log_ddl_command();
    |]

postgresTeardown :: (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue)) -> IO ()
postgresTeardown (testEnvironment, (server, _)) = do
  GraphqlEngine.postV2Query_ testEnvironment $
    [yaml|
type: run_sql
args:
  source: postgres
  sql: |
    DROP EVENT TRIGGER pg_get_ddl_command;

    DROP FUNCTION hasura.log_ddl_command;

    DROP TABLE hasura.ddl_history;
|]
  stopServer server
  Postgres.teardown schema (testEnvironment, ())

webhookServerMkLocalTestEnvironment ::
  TestEnvironment -> IO (GraphqlEngine.Server, Webhook.EventsQueue)
webhookServerMkLocalTestEnvironment _ = do
  Webhook.run

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests opts = do
  it "Creating an event trigger should create the SQL triggers" $ \(testEnvironment, (webhookServer, _)) -> do
    let webhookEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/hello"
    shouldReturnYaml
      opts
      ( GraphQLEngine.postMetadata
          testEnvironment
          [yaml|
type: pg_create_event_trigger
args:
  source: postgres
  table:
    schema: hasura
    name: users
  name: users_INSERT
  webhook: *webhookEndpoint
  insert:
    columns: "*"
          |]
      )
      [yaml|
message: success
           |]
  it "The source catalog should have been initialized along with the creation of the SQL trigger" $ \(testEnvironment, _) ->
    shouldReturnYaml
      opts
      ( GraphQLEngine.postV2Query
          200
          testEnvironment
          [yaml|
type: run_sql
args:
  source: postgres
  sql: |
      SELECT tag, object_identity FROM hasura.ddl_history ORDER BY object_identity;
          |]
      )
      [yaml|
result:
- - tag
  - object_identity
- - CREATE TABLE
  - hdb_catalog.event_invocation_logs
- - CREATE INDEX
  - hdb_catalog.event_invocation_logs_event_id_idx
- - CREATE TABLE
  - hdb_catalog.event_log
- - CREATE INDEX
  - hdb_catalog.event_log_fetch_events
- - CREATE INDEX
  - hdb_catalog.event_log_trigger_name_idx
- - CREATE FUNCTION
  - hdb_catalog.gen_hasura_uuid()
- - CREATE TABLE
  - hdb_catalog.hdb_source_catalog_version
- - CREATE INDEX
  - hdb_catalog.hdb_source_catalog_version_one_row
- - CREATE FUNCTION
  - hdb_catalog.insert_event_log(pg_catalog.text,pg_catalog.text,pg_catalog.text,pg_catalog.text,pg_catalog.json)
- - CREATE FUNCTION
  - hdb_catalog."notify_hasura_users_INSERT_INSERT"()
- - CREATE TRIGGER
  - '"notify_hasura_users_INSERT_INSERT" on hasura.users'
result_type: TuplesOk
|]
  it "only reloading the metadata should not recreate the SQL triggers" $ \(testEnvironment, _) -> do
    -- Truncate the ddl_history
    postV2Query_
      testEnvironment
      [yaml|
type: run_sql
args:
  source: postgres
  sql: TRUNCATE hasura.ddl_history RESTART IDENTITY;
|]
    shouldReturnYaml
      opts
      ( GraphQLEngine.postMetadata
          testEnvironment
          [yaml|
               type: reload_metadata
               args: {}
          |]
      )
      [yaml|
         is_consistent: true
         message: success
      |]
    shouldReturnYaml
      opts
      ( GraphQLEngine.postV2Query
          200
          testEnvironment
          [yaml|
               type: run_sql
               args:
                 source: postgres
                 sql:  SELECT tag, object_identity FROM hasura.ddl_history WHERE schema_name = 'hdb_catalog' ORDER BY object_identity;
           |]
      )
      [yaml|
           result:
           - - tag
             - object_identity
           result_type: TuplesOk
       |]
  it "reloading the metadata with `recreate_event_triggers: true` should recreate the SQL triggers" $ \(testEnvironment, _) -> do
    -- Truncate the ddl_history
    postV2Query_
      testEnvironment
      [yaml|
type: run_sql
args:
  source: postgres
  sql: TRUNCATE hasura.ddl_history RESTART IDENTITY;
|]
    shouldReturnYaml
      opts
      ( GraphQLEngine.postMetadata
          testEnvironment
          [yaml|
               type: reload_metadata
               args: {
                 "recreate_event_triggers": true
               }
          |]
      )
      [yaml|
         is_consistent: true
         message: success
      |]
    shouldReturnYaml
      opts
      ( GraphQLEngine.postV2Query
          200
          testEnvironment
          [yaml|
               type: run_sql
               args:
                 source: postgres
                 sql:  SELECT tag, object_identity FROM hasura.ddl_history WHERE schema_name = 'hdb_catalog' ORDER BY object_identity;
           |]
      )
      [yaml|
           result:
           - - tag
             - object_identity
           - - CREATE FUNCTION
             - hdb_catalog."notify_hasura_users_INSERT_INSERT"()
           result_type: TuplesOk
       |]
  it "adding a new column to the table should recreate the SQL trigger" $ \(testEnvironment, _) -> do
    -- Truncate the ddl_history
    postV2Query_
      testEnvironment
      [yaml|
type: run_sql
args:
  source: postgres
  sql: |
    TRUNCATE hasura.ddl_history RESTART IDENTITY;
    ALTER TABLE hasura.users ADD COLUMN last_name TEXT;
|]
    shouldReturnYaml
      opts
      ( GraphQLEngine.postV2Query
          200
          testEnvironment
          [yaml|
               type: run_sql
               args:
                 source: postgres
                 sql:  SELECT tag, object_identity FROM hasura.ddl_history WHERE schema_name = 'hdb_catalog' ORDER BY object_identity;
           |]
      )
      [yaml|
           result:
           - - tag
             - object_identity
           - - CREATE FUNCTION
             - hdb_catalog."notify_hasura_users_INSERT_INSERT"()
           - - CREATE FUNCTION
             - hdb_catalog."notify_hasura_users_INSERT_INSERT"()
           result_type: TuplesOk
       |]
