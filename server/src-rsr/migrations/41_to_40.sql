-- This migration checks for the type of the `locked` column
-- before applying the migration is due to an edge case. The
-- edge case is:
-- 1. User is using v1 hasura with no event triggers
-- 2. User upgrades to v2, since there are no event triggers in the metadata, source catalog migrations won't be run.
-- 3. User downgrades from v2 to v1, now since the 41_to_42 migration is
--    moved to `pg_source_migrations/0_to_1.sql` the
--    `hdb_catalog.event_log` exists as of the catalog version of v1.3.3,
--    which means we would be trying to downgrade something which was
--    never upgraded.
DO $$
BEGIN
  IF (SELECT EXISTS (SELECT 1 from information_schema.columns where table_name = 'event_log' and table_schema = 'hdb_catalog' and column_name = 'locked' and data_type = 'timestamp with time zone')) THEN
    ALTER TABLE hdb_catalog.event_log ALTER COLUMN locked TYPE BOOLEAN USING locked IS NOT NULL;
    ALTER TABLE hdb_catalog.event_log ALTER COLUMN locked SET NOT NULL;
    ALTER TABLE hdb_catalog.event_log ALTER COLUMN locked SET DEFAULT false;
  END IF;
END $$;
