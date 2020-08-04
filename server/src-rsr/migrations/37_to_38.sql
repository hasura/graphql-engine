CREATE FUNCTION hdb_catalog.current_setting(setting_name text) RETURNS json
  LANGUAGE plpgsql AS $$
  DECLARE
    server_version_num int;
    setting_value text;
  BEGIN
    server_version_num := current_setting('server_version_num');
    IF server_version_num >= 90600 THEN
      setting_value := current_setting('hasura.' || setting_name, true);
    ELSE
      -- We still support Postgres 9.5, which doesn’t support the second
      -- argument to current_setting, so emulate it by just catching the error.
      BEGIN
        setting_value := current_setting('hasura.' || setting_name);
      EXCEPTION WHEN OTHERS THEN
        setting_value := NULL;
      END;
    END IF;
    -- Note: we test for the empty string here to work around a Postgres bug.
    -- When SET LOCAL is used to set the value of a setting that does not
    -- currently exist, Postgres implicitly creates a /session-wide/ definition
    -- of the setting with the empty string as its value. This is annoying, but
    -- fortunately an empty string is not a legal JSON value, so it always means
    -- no value was set in the current transaction.
    IF setting_value IS NULL OR setting_value = '' THEN
      RETURN NULL;
    ELSE
      RETURN setting_value::json;
    END IF;
  END $$;

CREATE OR REPLACE FUNCTION hdb_catalog.insert_event_log(schema_name text, table_name text, trigger_name text, op text, row_data json)
  RETURNS text LANGUAGE plpgsql AS $$
  DECLARE
    id text;
    payload json;
  BEGIN
    id := gen_random_uuid();
    payload := json_build_object(
      'op', op,
      'data', row_data,
      'session_variables', hdb_catalog.current_setting('user'),
      'trace_context', hdb_catalog.current_setting('tracecontext')
    );
    INSERT INTO hdb_catalog.event_log
                (id, schema_name, table_name, trigger_name, payload)
      VALUES (id, schema_name, table_name, trigger_name, payload);
    RETURN id;
  END $$;
