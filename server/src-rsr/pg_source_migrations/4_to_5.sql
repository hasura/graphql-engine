-- The singleton expression index cannot be used as a replica identity.
-- A publication that includes UPDATEs requires one even for an INSERT with
-- ON CONFLICT DO UPDATE into an empty table (e.g. PostgreSQL 15.15).
ALTER TABLE hdb_catalog.hdb_source_catalog_version REPLICA IDENTITY FULL;
