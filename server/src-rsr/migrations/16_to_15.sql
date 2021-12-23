DELETE FROM hdb_catalog.hdb_table WHERE (table_schema, table_name)
IN (('hdb_catalog', 'hdb_query_collection'), ('hdb_catalog', 'hdb_allowlist'));

DROP TABLE IF EXISTS hdb_catalog.hdb_allowlist;
DROP TABLE IF EXISTS hdb_catalog.hdb_query_collection;