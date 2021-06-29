DROP TABLE hdb_catalog.hdb_remote_relationship;

DELETE FROM hdb_catalog.hdb_relationship
where table_schema = 'hdb_catalog' and table_name = 'hdb_remote_relationship';

DELETE FROM hdb_catalog.hdb_table
where table_schema = 'hdb_catalog' and table_name = 'hdb_remote_relationship';
