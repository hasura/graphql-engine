ALTER TABLE hdb_catalog.hdb_function
  ADD COLUMN configuration JSONB NOT NULL DEFAULT '{}'::jsonb;
