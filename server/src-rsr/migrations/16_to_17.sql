UPDATE hdb_catalog.hdb_table
  SET is_system_defined = 'true'
  WHERE table_schema = 'hdb_catalog' AND
        table_name = 'hdb_allowlist';
