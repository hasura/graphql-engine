ALTER TABLE hdb_catalog.hdb_version ADD COLUMN hasura_uuid UUID, ADD COLUMN misc_state JSONB;
UPDATE hdb_catalog.hdb_version SET hasura_uuid = gen_random_uuid();
