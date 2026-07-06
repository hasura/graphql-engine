ALTER TABLE hdb_catalog.hdb_function
  ADD COLUMN IF NOT EXISTS configuration JSONB NOT NULL DEFAULT '{}'::jsonb;
