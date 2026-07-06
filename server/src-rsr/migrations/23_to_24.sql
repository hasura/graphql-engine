ALTER TABLE hdb_catalog.hdb_table
ADD COLUMN IF NOT EXISTS configuration JSONB NOT NULL DEFAULT '{}'::jsonb;
