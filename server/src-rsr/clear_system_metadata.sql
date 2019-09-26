DELETE FROM hdb_catalog.hdb_function WHERE is_system_defined = 'true';
DELETE FROM hdb_catalog.hdb_permission WHERE is_system_defined = 'true';
DELETE FROM hdb_catalog.hdb_relationship WHERE is_system_defined = 'true';
DELETE FROM hdb_catalog.hdb_table WHERE is_system_defined = 'true';
DELETE FROM hdb_catalog.hdb_query_collection WHERE is_system_defined = 'true';
