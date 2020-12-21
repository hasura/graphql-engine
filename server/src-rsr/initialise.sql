/* We define our own uuid generator function that uses gen_random_uuid() underneath.
Since the column default is not directly referencing gen_random_uuid(),
it prevents the column default to be dropped when pgcrypto or public schema is dropped unwittingly.

See https://github.com/hasura/graphql-engine/issues/4217
*/
CREATE FUNCTION hdb_catalog.gen_hasura_uuid() RETURNS uuid AS
-- We assume gen_random_uuid() is available in the search_path.
-- This may not be true but we can't do much till https://github.com/hasura/graphql-engine/issues/3657
'select gen_random_uuid()' LANGUAGE SQL;

CREATE TABLE hdb_catalog.hdb_version (
    hasura_uuid UUID PRIMARY KEY DEFAULT hdb_catalog.gen_hasura_uuid(),
    version TEXT NOT NULL,
    upgraded_on TIMESTAMPTZ NOT NULL,
    cli_state JSONB NOT NULL DEFAULT '{}'::jsonb,
    console_state JSONB NOT NULL DEFAULT '{}'::jsonb
);

CREATE UNIQUE INDEX hdb_version_one_row
ON hdb_catalog.hdb_version((version IS NOT NULL));

-- Create table which stores metadata JSON blob
CREATE TABLE hdb_catalog.hdb_metadata
(
  id INTEGER PRIMARY KEY,
  metadata JSON NOT NULL
);

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

CREATE INDEX ON hdb_catalog.event_log (trigger_name);
CREATE INDEX ON hdb_catalog.event_log (locked);
CREATE INDEX ON hdb_catalog.event_log (delivered);
CREATE INDEX ON hdb_catalog.event_log (created_at);

CREATE TABLE hdb_catalog.event_invocation_logs
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMP DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.event_log (id)
);

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

CREATE TABLE hdb_catalog.hdb_action_log
(
  id UUID PRIMARY KEY DEFAULT hdb_catalog.gen_hasura_uuid(),
  -- we deliberately do not reference the action name
  -- because sometimes we may want to retain history
  -- after dropping the action
  action_name TEXT,
  input_payload JSONB NOT NULL,
  request_headers JSONB NOT NULL,
  session_variables JSONB NOT NULL,
  response_payload JSONB NULL,
  errors JSONB NULL,
  created_at timestamptz NOT NULL default now(),
  response_received_at timestamptz NULL,
  status text NOT NULL,
  CHECK (status IN ('created', 'processing', 'completed', 'error'))
);

CREATE TABLE hdb_catalog.hdb_cron_events
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  trigger_name TEXT NOT NULL,
  scheduled_time TIMESTAMPTZ NOT NULL,
  status TEXT NOT NULL DEFAULT 'scheduled',
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  next_retry_at TIMESTAMPTZ,

  CONSTRAINT valid_status CHECK (status IN ('scheduled','locked','delivered','error','dead'))
);

CREATE INDEX hdb_cron_event_status ON hdb_catalog.hdb_cron_events (status);

CREATE TABLE hdb_catalog.hdb_cron_event_invocation_logs
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMPTZ DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.hdb_cron_events (id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE hdb_catalog.hdb_scheduled_events
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  webhook_conf JSON NOT NULL,
  scheduled_time TIMESTAMPTZ NOT NULL,
  retry_conf JSON,
  payload JSON,
  header_conf JSON,
  status TEXT NOT NULL DEFAULT 'scheduled',
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  next_retry_at TIMESTAMPTZ,
  comment TEXT,
  CONSTRAINT valid_status CHECK (status IN ('scheduled','locked','delivered','error','dead'))
);

CREATE INDEX hdb_scheduled_event_status ON hdb_catalog.hdb_scheduled_events (status);

CREATE TABLE hdb_catalog.hdb_scheduled_event_invocation_logs
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMPTZ DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.hdb_scheduled_events (id)
     ON DELETE CASCADE ON UPDATE CASCADE
);
