DROP VIEW hdb_catalog.hdb_role;

CREATE VIEW hdb_catalog.hdb_role AS
(
SELECT DISTINCT role_name FROM (
SELECT role_name FROM hdb_catalog.hdb_permission
UNION ALL
SELECT role_name FROM hdb_catalog.hdb_action_permission
) q
);

DROP TABLE hdb_catalog.hdb_remote_schema_permission;

DELETE FROM hdb_catalog.hdb_relationship where table_schema = 'hdb_catalog' and table_name = 'remote_schemas';
DELETE FROM hdb_catalog.hdb_relationship where table_schema = 'hdb_catalog' and table_name = 'hdb_role' and rel_name = 'remote_schema_permissions';
DELETE FROM hdb_catalog.hdb_table where table_schema        = 'hdb_catalog' and table_name = 'hdb_remote_schema_permission';
