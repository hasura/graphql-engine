CREATE OR REPLACE function hdb_views.notify_skor_{{NAME}}_{{OPERATION}}() RETURNS trigger
   LANGUAGE plpgsql
   AS $$
   DECLARE
   payload json;
   data json;
   BEGIN
   raise notice 'pushing to event log';
   data := json_build_object(
     'old', {{OLD_DATA_EXPRESSION}},
     'new', {{NEW_DATA_EXPRESSION}}
   );
   payload := json_build_object(
                        'table', TG_TABLE_NAME,
                        'schema', TG_TABLE_SCHEMA,
                        'op', TG_OP,
                        'data', data
                        )::text;
   INSERT INTO
   hdb_catalog.event_log (schema_name, table_name, trigger_name, payload)
   VALUES
     (TG_TABLE_SCHEMA, TG_TABLE_NAME, '{{NAME}}', payload);
   RETURN NULL;
   END;
   $$;
   DROP TRIGGER IF EXISTS notify_skor_{{NAME}}_{{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}};
   CREATE TRIGGER notify_skor_{{NAME}}_{{OPERATION}} AFTER {{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}} FOR EACH ROW EXECUTE PROCEDURE hdb_views.notify_skor_{{NAME}}_{{OPERATION}}();

