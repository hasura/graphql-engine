ALTER TABLE hdb_catalog.event_triggers
  ADD CONSTRAINT event_triggers_schema_name_table_name_fkey
    FOREIGN KEY (schema_name, table_name)
    REFERENCES hdb_catalog.hdb_table(table_schema, table_name)
    ON UPDATE CASCADE;
