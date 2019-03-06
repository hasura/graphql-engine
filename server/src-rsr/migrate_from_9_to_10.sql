ALTER TABLE hdb_catalog.hdb_relationship
  DROP CONSTRAINT hdb_relationship_table_schema_fkey,
  ADD CONSTRAINT hdb_relationship_table_schema_fkey FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE;

ALTER TABLE hdb_catalog.hdb_permission
  DROP CONSTRAINT hdb_permission_table_schema_fkey,
  ADD CONSTRAINT hdb_permission_table_schema_fkey FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE;

ALTER TABLE hdb_catalog.event_triggers
ADD CONSTRAINT event_triggers_table_schema_fkey FOREIGN KEY (schema_name, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE;
