CREATE OR REPLACE FUNCTION
  hdb_catalog.insert_event_log(schema_name text, table_name text, trigger_name text, op text, row_data json)
  RETURNS text AS $$
  DECLARE
    id text;
    payload json;
    session_variables json;
    server_version_num int;
    trace_context json;
    trace_context_text text;
  BEGIN
    id := gen_random_uuid();
    server_version_num := current_setting('server_version_num');
    trace_context_text := current_setting('hasura.tracecontext', 't');
    IF server_version_num >= 90600 THEN
      session_variables := current_setting('hasura.user', 't');
      -- See graphql-engine/issues/5542
      -- We need to check trace_context_text != '' because of a Postgres bug
      IF trace_context_text != '' THEN
        trace_context := trace_context_text;
      ELSE
        trace_context := NULL;
      END IF;
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