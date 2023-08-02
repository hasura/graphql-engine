{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.EventTriggers.PG.EventTriggersRecreationSpec (spec) where

import Data.Aeson.Types qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postV2Query_)
import Harness.GraphqlEngine qualified as GraphQLEngine
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Webhook qualified as Webhook
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it, shouldContain)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironmentSingleSetup
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = const Webhook.runEventsWebhook,
              Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  Fixture.SetupAction
                    { Fixture.setupAction = postgresSetup testEnvironment,
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

postgresSetup :: TestEnvironment -> IO ()
postgresSetup testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
  -- In the setup, we create postgres's event triggers that capture every DDL
  -- change made in the database and then store them in a table called
  -- `ddl_history` that contains metadata about the DDL query like
  -- the query that was executed, time at which the query was executed,
  -- what type of query it was etc.
  GraphqlEngine.postV2Query_
    testEnvironment
    [interpolateYaml|
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

           DROP TABLE IF EXISTS hdb_catalog.hdb_event_log_cleanups;

           CREATE TABLE #{schemaName}.ddl_history (
             id serial primary key,
             event text,
             tag text,
             object_type text,
             schema_name text,
             object_identity text,
             query text,
             created_at timestamptz default now()
           );

           CREATE or REPLACE FUNCTION #{schemaName}.log_ddl_command()
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
               insert into #{schemaName}.ddl_history(
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

           CREATE EVENT TRIGGER pg_get_ddl_command on ddl_command_end EXECUTE PROCEDURE #{schemaName}.log_ddl_command();
    |]

postgresTeardown :: TestEnvironment -> IO ()
postgresTeardown testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment
  GraphqlEngine.postV2Query_ testEnvironment
    $ [interpolateYaml|
type: run_sql
args:
  source: postgres
  sql: |
    DROP EVENT TRIGGER pg_get_ddl_command;

    DROP FUNCTION #{schemaName}.log_ddl_command;

    DROP TABLE #{schemaName}.ddl_history;
|]

--------------------------------------------------------------------------------

-- * Tests

tests :: SpecWith (TestEnvironment, (GraphqlEngine.Server, Webhook.EventsQueue))
tests = do
  it "Creating an event trigger should create the SQL triggers" $ \(testEnvironment, (webhookServer, _)) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment
        webhookEndpoint = GraphqlEngine.serverUrl webhookServer ++ "/hello"
    shouldReturnYaml
      testEnvironment
      ( GraphQLEngine.postMetadata
          testEnvironment
          [interpolateYaml|
type: pg_create_event_trigger
args:
  source: postgres
  table:
    schema: #{schemaName}
    name: users
  name: users_INSERT
  webhook: #{webhookEndpoint}
  insert:
    columns: "*"
          |]
      )
      [interpolateYaml|
message: success
           |]
  it "The source catalog should have been initialized along with the creation of the SQL trigger" $ \(testEnvironment, _) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment
    -- fetch `$.result` from the result / expectation so we can compare array
    -- membership
    let getResult :: Aeson.Value -> [Aeson.Value]
        getResult = fromMaybe mempty . parse
          where
            parse =
              ( Aeson.parseMaybe $ \case
                  (Aeson.Object o) -> o Aeson..: "result"
                  _ -> mempty
              )

    result <-
      GraphQLEngine.postV2Query
        200
        testEnvironment
        [interpolateYaml|
            type: run_sql
            args:
              source: postgres
              sql: |
                  SELECT tag, object_identity FROM #{schemaName}.ddl_history ORDER BY object_identity COLLATE "C";
          |]

    let expected =
          [interpolateYaml|
        result:
        - - tag
          - object_identity
        - - CREATE TRIGGER
          - '"notify_#{schemaName}_users_INSERT_INSERT" on #{schemaName}.users'
        - - CREATE SCHEMA
          - "hdb_catalog"
        - - CREATE FUNCTION
          - hdb_catalog."notify_#{schemaName}_users_INSERT_INSERT"()
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
          - hdb_catalog.hdb_event_log_cleanups
        - - CREATE TABLE
          - hdb_catalog.hdb_source_catalog_version
        - - CREATE INDEX
          - hdb_catalog.hdb_source_catalog_version_one_row
        - - CREATE FUNCTION
          - hdb_catalog.insert_event_log(pg_catalog.text,pg_catalog.text,pg_catalog.text,pg_catalog.text,pg_catalog.json)
      |]

    getResult result `shouldContain` getResult expected

  it "only reloading the metadata should not recreate the SQL triggers" $ \(testEnvironment, _) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment
    -- Truncate the ddl_history
    postV2Query_
      testEnvironment
      [interpolateYaml|
type: run_sql
args:
  source: postgres
  sql: TRUNCATE #{schemaName}.ddl_history RESTART IDENTITY;
|]
    shouldReturnYaml
      testEnvironment
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
      testEnvironment
      ( GraphQLEngine.postV2Query
          200
          testEnvironment
          [interpolateYaml|
               type: run_sql
               args:
                 source: postgres
                 sql:  SELECT tag, object_identity FROM #{schemaName}.ddl_history WHERE schema_name = 'hdb_catalog' ORDER BY object_identity;
           |]
      )
      [yaml|
           result:
           - - tag
             - object_identity
           result_type: TuplesOk
       |]
  it "reloading the metadata with `recreate_event_triggers: true` should recreate the SQL triggers" $ \(testEnvironment, _) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment
    -- Truncate the ddl_history
    postV2Query_
      testEnvironment
      [interpolateYaml|
type: run_sql
args:
  source: postgres
  sql: TRUNCATE #{schemaName}.ddl_history RESTART IDENTITY;
|]
    shouldReturnYaml
      testEnvironment
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
      testEnvironment
      ( GraphQLEngine.postV2Query
          200
          testEnvironment
          [interpolateYaml|
               type: run_sql
               args:
                 source: postgres
                 sql:  SELECT tag, object_identity FROM #{schemaName}.ddl_history WHERE schema_name = 'hdb_catalog' ORDER BY object_identity;
           |]
      )
      [interpolateYaml|
           result:
           - - tag
             - object_identity
           - - CREATE FUNCTION
             - hdb_catalog."notify_#{schemaName}_users_INSERT_INSERT"()
           result_type: TuplesOk
       |]
  it "adding a new column to the table should recreate the SQL trigger" $ \(testEnvironment, _) -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment
    -- Truncate the ddl_history
    postV2Query_
      testEnvironment
      [interpolateYaml|
type: run_sql
args:
  source: postgres
  sql: |
    TRUNCATE #{schemaName}.ddl_history RESTART IDENTITY;
    ALTER TABLE #{schemaName}.users ADD COLUMN last_name TEXT;
|]
    shouldReturnYaml
      testEnvironment
      ( GraphQLEngine.postV2Query
          200
          testEnvironment
          [interpolateYaml|
               type: run_sql
               args:
                 source: postgres
                 sql:  SELECT tag, object_identity FROM #{schemaName}.ddl_history WHERE schema_name = 'hdb_catalog' ORDER BY object_identity;
           |]
      )
      [interpolateYaml|
           result:
           - - tag
             - object_identity
           - - CREATE FUNCTION
             - hdb_catalog."notify_#{schemaName}_users_INSERT_INSERT"()
           - - CREATE FUNCTION
             - hdb_catalog."notify_#{schemaName}_users_INSERT_INSERT"()
           result_type: TuplesOk
       |]
