CREATE TABLE hdb_catalog.hdb_remote_relationship
(
remote_relationship_name TEXT NOT NULL,
table_schema name NOT NULL,
table_name name NOT NULL,
definition JSONB NOT NULL,
PRIMARY KEY (remote_relationship_name, table_schema, table_name),
FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE
);
