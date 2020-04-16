DROP VIEW  hdb_catalog.hdb_role;
DROP TABLE hdb_catalog.hdb_action_permission;
DROP TABLE hdb_catalog.hdb_action;
DROP TABLE hdb_catalog.hdb_custom_types;
DROP TABLE hdb_catalog.hdb_action_log;
DELETE FROM hdb_catalog.hdb_relationship where table_schema = 'hdb_catalog' and table_name in ('hdb_action', 'hdb_action_permission', 'hdb_action_log', 'hdb_custom_types', 'hdb_role');
DELETE FROM hdb_catalog.hdb_table where table_schema = 'hdb_catalog' and table_name in ('hdb_action', 'hdb_action_permission', 'hdb_action_log', 'hdb_custom_types', 'hdb_role');
