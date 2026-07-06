ALTER TABLE hdb_catalog.hdb_version
  ADD COLUMN IF NOT EXISTS hasura_uuid UUID DEFAULT gen_random_uuid(),
  ADD COLUMN IF NOT EXISTS cli_state JSONB NOT NULL DEFAULT '{}'::jsonb,
  ADD COLUMN IF NOT EXISTS console_state JSONB NOT NULL DEFAULT '{}'::jsonb,
  ADD CONSTRAINT hasura_uuid_pkey PRIMARY KEY (hasura_uuid);
