CREATE TABLE hdb_catalog.hdb_query_template
(
  template_name TEXT PRIMARY KEY,
  template_defn JSONB NOT NULL,
  comment    TEXT NULL,
  is_system_defined boolean default false
);

INSERT INTO hdb_catalog.hdb_table (table_schema, table_name)
            VALUES ('hdb_catalog', 'hdb_query_template');