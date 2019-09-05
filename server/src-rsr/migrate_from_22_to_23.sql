CREATE TABLE hdb_catalog.remote_schema_permissions
  (
    remote_schema TEXT,
    role TEXT,
    definition JSONB NOT NULL,
    PRIMARY KEY (remote_schema, role),
    FOREIGN KEY (remote_schema) REFERENCES hdb_catalog.remote_schemas(name) ON UPDATE CASCADE
  );

DROP VIEW hdb_catalog.hdb_permission_agg;

CREATE VIEW hdb_catalog.hdb_permission_agg AS
  ( SELECT
      table_schema,
      table_name,
      null as remote_schema,
      role_name,
      json_object_agg(perm_type, perm_def) as permissions
     FROM
       hdb_catalog.hdb_permission
     GROUP BY
       table_schema, table_name, role_name
  )
  UNION ALL
  ( SELECT
      null as table_schema,
      null as table_name,
      remote_schema,
      role as role_name,
      definition::json as permissions
    FROM
      hdb_catalog.remote_schema_permissions
  );
