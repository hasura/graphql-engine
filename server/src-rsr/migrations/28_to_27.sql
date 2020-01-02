ALTER TABLE hdb_catalog.hdb_function
  DROP COLUMN configuration;

UPDATE hdb_catalog.hdb_version
  SET version = '27'
  WHERE version = '28';
