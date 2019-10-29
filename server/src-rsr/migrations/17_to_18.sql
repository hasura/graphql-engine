DELETE FROM hdb_catalog.hdb_table
WHERE table_schema = 'hdb_catalog'
  AND table_name = 'hdb_query_template';
DROP table hdb_catalog.hdb_query_template
