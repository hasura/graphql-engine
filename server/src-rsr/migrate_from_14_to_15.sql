CREATE OR REPLACE FUNCTION
  hdb_catalog.insert_event_log(schema_name text, table_name text, trigger_name text, op text, row_data json)
  RETURNS text AS $$
  DECLARE
  id text;
  payload json;
  session_variables json;
  server_version_num int;
BEGIN
  id := gen_random_uuid();
  server_version_num := current_setting('server_version_num');
  IF server_version_num >= 90600 THEN
    session_variables := current_setting('hasura.user', 't');
  ELSE
    BEGIN
      session_variables := current_setting('hasura.user');
    EXCEPTION WHEN OTHERS THEN
      session_variables := NULL;
    END;
  END IF;
  payload := json_build_object(
    'op', op,
    'data', row_data,
    'session_variables', session_variables
  );
  INSERT INTO hdb_catalog.event_log
    (id, schema_name, table_name, trigger_name, payload)
  VALUES
    (id, schema_name, table_name, trigger_name, payload);
  RETURN id;
END;
$$ LANGUAGE plpgsql;
