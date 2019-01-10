ALTER TABLE hdb_catalog.hdb_version
    ADD COLUMN hasura_uuid UUID DEFAULT gen_random_uuid(),
    ADD COLUMN misc_state JSONB NOT NULL DEFAULT '{}'::jsonb,
    ADD CONSTRAINT hasura_uuid_pkey PRIMARY KEY (hasura_uuid);