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
      -- In some cases postgres sets the setting to an empty string, which is not a valid json.
      -- NULLIF will convert the empty string to NULL.
      -- Ref: https://github.com/hasura/graphql-engine/issues/8498
      session_variables := NULLIF(current_setting('hasura.user', 't'), '');
      trace_context := NULLIF(current_setting('hasura.tracecontext', 't'), '');
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