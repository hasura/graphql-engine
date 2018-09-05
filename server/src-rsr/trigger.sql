CREATE OR REPLACE function hdb_views.notify_hasura_{{NAME}}_{{OPERATION}}() RETURNS trigger
   LANGUAGE plpgsql
   AS $$
   DECLARE
   payload json;
   data json;
   id text;
   BEGIN
     id := gen_random_uuid();
     data := json_build_object(
       'old', {{OLD_DATA_EXPRESSION}},
       'new', {{NEW_DATA_EXPRESSION}}
     );
     payload := json_build_object(
                        'table', TG_TABLE_NAME,
                        'schema', TG_TABLE_SCHEMA,
                        'trigger_name', '{{NAME}}',
                        'trigger_id', '{{ID}}',
                        'op', TG_OP,
                        'id', id,
                        'data', data
                        )::text;
     INSERT INTO
     hdb_catalog.event_log (id, schema_name, table_name, trigger_name, trigger_id, payload)
     VALUES
     (id, TG_TABLE_SCHEMA, TG_TABLE_NAME, '{{NAME}}', '{{ID}}', payload);
     RETURN NULL;
   END;
   $$;
   DROP TRIGGER IF EXISTS notify_hasura_{{NAME}}_{{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}};
   CREATE TRIGGER notify_hasura_{{NAME}}_{{OPERATION}} AFTER {{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}} FOR EACH ROW EXECUTE PROCEDURE hdb_views.notify_hasura_{{NAME}}_{{OPERATION}}();
