CREATE TABLE hdb_catalog.hdb_remote_schema_permission
  (
    remote_schema TEXT,
    role_name TEXT,
    definition JSONB NOT NULL,
    PRIMARY KEY (remote_schema, role_name),
    FOREIGN KEY (remote_schema) REFERENCES hdb_catalog.remote_schemas(name) ON UPDATE CASCADE
  );

CREATE VIEW hdb_catalog.hdb_roles AS
  (SELECT DISTINCT(role_name) FROM hdb_catalog.hdb_permission_agg)
  UNION
  (SELECT DISTINCT(role_name) FROM hdb_catalog.hdb_remote_schema_permission);
