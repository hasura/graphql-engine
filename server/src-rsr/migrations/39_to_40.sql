ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT IF EXISTS event_triggers_schema_name_fkey;

ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT IF EXISTS event_triggers_schema_name_table_name_fkey;

-- since we removed the foreign key constraint with hdb_catalog.hdb_table which had 'ON UPDATE CASCADE',
-- we perform the update using trigger
CREATE OR REPLACE FUNCTION hdb_catalog.event_trigger_table_name_update()
RETURNS TRIGGER
LANGUAGE PLPGSQL
AS
$$
BEGIN
  IF (NEW.table_schema, NEW.table_name) <> (OLD.table_schema, OLD.table_name)  THEN
    UPDATE hdb_catalog.event_triggers
    SET schema_name = NEW.table_schema, table_name = NEW.table_name
    WHERE (schema_name, table_name) = (OLD.table_schema, OLD.table_name);
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER event_trigger_table_name_update_trigger
AFTER UPDATE ON hdb_catalog.hdb_table
FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.event_trigger_table_name_update();


-- this clears pre-existing invalid triggers
CREATE TEMP TABLE invalid_triggers AS
WITH valid_event_triggers AS (
  SELECT name FROM hdb_catalog.event_triggers
),
  archive_invalid_events AS (
  UPDATE hdb_catalog.event_log set archived = 't'
  WHERE trigger_name NOT IN (select name from valid_event_triggers)
)
SELECT routine_name FROM information_schema.routines
WHERE routine_type='FUNCTION' AND specific_schema='hdb_views' AND routine_name NOT IN (
  SELECT left('notify_hasura_' || name || '_INSERT', 63) FROM valid_event_triggers -- trigger names are truncated to 63 chars
  UNION
  SELECT left('notify_hasura_' || name || '_UPDATE', 63) FROM valid_event_triggers
  UNION
  select left('notify_hasura_' || name || '_DELETE', 63) FROM valid_event_triggers
);

DO $$ DECLARE
r RECORD;
BEGIN
  FOR r IN (SELECT routine_name from invalid_triggers) LOOP
    EXECUTE 'DROP FUNCTION IF EXISTS hdb_views.' || quote_ident(r.routine_name) || '() CASCADE'; -- without '()' here, PG < 10 will throw error
  END LOOP;
END $$;
