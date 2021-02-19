-- This migration enables metadata versioning
-- Drops version column

ALTER TABLE hdb_catalog.hdb_metadata DROP COLUMN "resource_version";
