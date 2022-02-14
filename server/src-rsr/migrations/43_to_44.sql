-- This migration adds versioning to metadata, used for optimistic locking in UIs.
-- TODO: Are there changes required in catalog_versions.txt

DO $$ 
    BEGIN
        BEGIN
          ALTER TABLE hdb_catalog.hdb_metadata ADD COLUMN "resource_version" INTEGER NOT NULL DEFAULT 1 UNIQUE;
        EXCEPTION
          -- For pg 9.6 compatibility
          WHEN duplicate_column THEN RAISE NOTICE 'column resource_version already exists in hdb_metadata';
      END;
  END;
$$
