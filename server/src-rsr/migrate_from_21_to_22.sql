CREATE TABLE hdb_catalog.remote_schema_permissions
  (
    remote_schema TEXT,
    role TEXT,
    definition JSONB NOT NULL,
    PRIMARY KEY (remote_schema, role),
    FOREIGN KEY (remote_schema) REFERENCES hdb_catalog.remote_schemas(name) ON UPDATE CASCADE
  );
