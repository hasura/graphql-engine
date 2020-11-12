DROP TRIGGER event_trigger_table_name_update_trigger ON hdb_catalog.hdb_table;

DROP FUNCTION hdb_catalog.event_trigger_table_name_update();

ALTER TABLE hdb_catalog.event_triggers
ADD CONSTRAINT event_triggers_schema_name_table_name_fkey
FOREIGN KEY (schema_name, table_name)
REFERENCES hdb_catalog.hdb_table(table_schema, table_name)
ON UPDATE CASCADE;
