-- This migration adds versioning to metadata, used for optimistic locking in UIs.
-- TODO: Are there changes required in catalog_versions.txt

ALTER TABLE hdb_catalog.hdb_metadata ADD COLUMN "resource_version" INTEGER NOT NULL DEFAULT 1 UNIQUE;
