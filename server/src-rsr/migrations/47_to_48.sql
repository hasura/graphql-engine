ALTER TABLE hdb_catalog.hdb_version
  ADD COLUMN IF NOT EXISTS ee_client_id TEXT,
  ADD COLUMN IF NOT EXISTS ee_client_secret TEXT;
