ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT event_triggers_schema_name_table_name_fkey;

-- since we removed the foreign key constraint with hdb_catalog.hdb_table which had 'ON UPDATE CASCADE'
-- (see Note [Diff-and-patch event triggers on replace] in Hasura.RQL.DDL.EventTrigger), we perform the update using trigger
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
