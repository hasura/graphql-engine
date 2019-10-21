ALTER TABLE hdb_catalog.hdb_table
ADD COLUMN configuration JSONB NOT NULL DEFAULT '{}'::jsonb;
