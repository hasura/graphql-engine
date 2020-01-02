ALTER TABLE hdb_catalog.hdb_table DROP COLUMN configuration;

UPDATE hdb_catalog.hdb_version
   SET version = '23'
 WHERE version = '24';

