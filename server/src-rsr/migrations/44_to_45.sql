-- This migration adds the schema notification table
--
-- NOTE: In OSS this table only contains a single row (indicated by ID 1).
--       This may change to allow multiple notifications in future.
CREATE TABLE IF NOT EXISTS hdb_catalog.hdb_schema_notifications
(
  id INTEGER PRIMARY KEY CHECK (id = 1),
  notification JSON NOT NULL,
  resource_version INTEGER NOT NULL DEFAULT 1,
  instance_id UUID NOT NULL,
  updated_at TIMESTAMPTZ DEFAULT NOW()
);
