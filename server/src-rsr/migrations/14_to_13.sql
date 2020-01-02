DROP VIEW IF EXISTS hdb_catalog.hdb_table_info_agg;
DROP VIEW IF EXISTS hdb_catalog.hdb_function_info_agg;

UPDATE hdb_catalog.hdb_version
  SET version = '13'
WHERE version = '14';


