CREATE TABLE hdb_catalog.hdb_remote_schema_permission (
remote_schema_name text,
role_name text,
definition jsonb,
comment text,
PRIMARY KEY (remote_schema_name, role_name)
);
