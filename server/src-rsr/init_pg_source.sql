/* We define our own uuid generator function that uses gen_random_uuid() underneath.
   Since the column default is not directly referencing gen_random_uuid(),
   it prevents the column default to be dropped when pgcrypto or public schema is dropped unwittingly.

   See https://github.com/hasura/graphql-engine/issues/4217

   There is another instance of this function, defined in `initialise.sql`. We
   need to define them in both places because the `gen_hasura_uuid` function is
   used as column defaults for various tables stored in both the metadata
   database and the event log table in user's (source) database. In the case
   where the metadata database is separate from the source database, we need to
   create these functions separately. Note that both of these definitions have
   to be the same.
 */
CREATE OR REPLACE FUNCTION hdb_catalog.gen_hasura_uuid() RETURNS uuid AS
  -- We assume gen_random_uuid() is available in the search_path.
  -- This may not be true but we can't do much till https://github.com/hasura/graphql-engine/issues/3657
'select gen_random_uuid()' LANGUAGE SQL;

CREATE TABLE hdb_catalog.hdb_source_catalog_version(
  version TEXT NOT NULL,
  upgraded_on TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX hdb_source_catalog_version_one_row
ON hdb_catalog.hdb_source_catalog_version((version IS NOT NULL));

/* TODO: The columns `created_at` and `next_retry_at` does not contain timezone (TIMESTAMP type) while `locked` has a timezone
offset (TIMESTAMPTZ). The time repesented by TIMESTAMP is in the timezone of the Postgres server. If the
timezone of the PG server is changed, then the entries in the event_log table can be confusing since there is no
timezone offset to highlight the difference. A possible solution to it is to change the type of the two columns to
include the timezone offset and keep all the times in UTC. However, altering a column type is a time
taking process, hence not migrating the source to add a timezone offset */
CREATE TABLE hdb_catalog.event_log
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  trigger_name TEXT NOT NULL,
  payload JSONB NOT NULL,
  delivered BOOLEAN NOT NULL DEFAULT FALSE,
  error BOOLEAN NOT NULL DEFAULT FALSE,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  /* when locked IS NULL the event is unlocked and can be processed */
  locked TIMESTAMPTZ,
  next_retry_at TIMESTAMP,
  archived BOOLEAN NOT NULL DEFAULT FALSE
);

/* This powers `archiveEvents` */
CREATE INDEX ON hdb_catalog.event_log (trigger_name);
/* This index powers `fetchEvents` */
CREATE INDEX event_log_fetch_events
  ON hdb_catalog.event_log (locked NULLS FIRST, next_retry_at NULLS FIRST, created_at)
  WHERE delivered = 'f'
    and error = 'f'
    and archived = 'f'
;


CREATE TABLE hdb_catalog.event_invocation_logs
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  trigger_name TEXT,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMP DEFAULT NOW()
);

/* This index improves the performance of deletes by event_id, so that if somebody
tries to delete an event from the hdb_catalog.event_log along with the invocation log
it will be faster with an index compared to without an index. */
CREATE INDEX ON hdb_catalog.event_invocation_logs (event_id);

CREATE OR REPLACE FUNCTION
  hdb_catalog.insert_event_log(schema_name text, table_name text, trigger_name text, op text, row_data json)
  RETURNS text AS $$
  DECLARE
    id text;
    payload json;
    session_variables json;
    server_version_num int;
    trace_context json;
  BEGIN
    id := gen_random_uuid();
    server_version_num := current_setting('server_version_num');
    IF server_version_num >= 90600 THEN
      session_variables := current_setting('hasura.user', 't');
      trace_context := current_setting('hasura.tracecontext', 't');
    ELSE
      BEGIN
        session_variables := current_setting('hasura.user');
      EXCEPTION WHEN OTHERS THEN
                  session_variables := NULL;
      END;
      BEGIN
        trace_context := current_setting('hasura.tracecontext');
      EXCEPTION WHEN OTHERS THEN
        trace_context := NULL;
      END;
    END IF;
    payload := json_build_object(
      'op', op,
      'data', row_data,
      'session_variables', session_variables,
      'trace_context', trace_context
    );
    INSERT INTO hdb_catalog.event_log
                (id, schema_name, table_name, trigger_name, payload)
    VALUES
    (id, schema_name, table_name, trigger_name, payload);
    RETURN id;
  END;
$$ LANGUAGE plpgsql;

CREATE TABLE hdb_catalog.hdb_event_log_cleanups
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  trigger_name TEXT NOT NULL,
  scheduled_at TIMESTAMP NOT NULL,
  deleted_event_logs INTEGER,
  deleted_event_invocation_logs INTEGER,
  status TEXT NOT NULL,
  CHECK (status IN ('scheduled', 'paused', 'completed', 'dead')),

  UNIQUE (trigger_name, scheduled_at)
);
