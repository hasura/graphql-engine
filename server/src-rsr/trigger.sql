CREATE OR REPLACE function hdb_catalog.notify_skor_{{NAME}}_{{OPERATION}}() RETURNS trigger
   LANGUAGE plpgsql
   AS $$
   DECLARE
   payload json;
   BEGIN
   raise notice 'pushing to event log';
   payload := json_build_object(
                        'table', TG_TABLE_NAME,
                        'schema', TG_TABLE_SCHEMA,
                        'op', TG_OP,
                        'data', {{DATA_EXPRESSION}}
                        )::text;
   INSERT INTO
   hdb_catalog.event_log (trigger_name, payload, webhook)
   SELECT e.name, payload, e.webhook
   FROM hdb_catalog.event_triggers e
   WHERE e.name='{{NAME}}';
   RETURN NULL;
   END;
   $$;
   DROP TRIGGER IF EXISTS notify_skor_{{NAME}}_{{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}};
   CREATE TRIGGER notify_skor_{{NAME}}_{{OPERATION}} AFTER {{OPERATION}} ON {{SCHEMA_NAME}}.{{TABLE_NAME}} FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.notify_skor_{{NAME}}_{{OPERATION}}();

