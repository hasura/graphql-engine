UPDATE hdb_catalog.hdb_table
  SET is_system_defined = 'false'
WHERE table_schema = 'hdb_catalog'
      AND  table_name = 'hdb_allowlist';

UPDATE hdb_catalog.hdb_version
  SET version = '16'
WHERE version = '17';

