CREATE TABLE hdb_catalog.hdb_remote_relationship
  (
    name TEXT NOT NULL,
    table_schema TEXT NOT NULL,
    table_name TEXT NOT NULL,
    remote_schema TEXT NOT NULL,
    configuration JSONB NOT NULL,
    PRIMARY KEY (name, table_schema, table_name),
    FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE
  );

