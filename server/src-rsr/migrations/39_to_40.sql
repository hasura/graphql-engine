CREATE TABLE hdb_catalog.hdb_remote_schema_permission (
remote_schema_name text,
role_name text,
definition jsonb,
comment text,
PRIMARY KEY (remote_schema_name, role_name)
);

DROP VIEW hdb_catalog.hdb_role;

CREATE VIEW hdb_catalog.hdb_role AS
(
SELECT DISTINCT role_name FROM (
SELECT role_name FROM hdb_catalog.hdb_permission
UNION ALL
SELECT role_name FROM hdb_catalog.hdb_action_permission
UNION ALL
SELECT role_name FROM hdb_catalog.hdb_remote_schema_permission
) q
);
