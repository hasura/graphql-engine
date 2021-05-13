-- This index is added to avoid creating duplicate cron
-- events when multiple Hasura instances are running.
-- This is a partial index for backwards compatibility i.e.
-- the metadata db might already have duplicated events before this change was added.
CREATE UNIQUE INDEX hdb_cron_events_unique_scheduled
ON hdb_catalog.hdb_cron_events (trigger_name, scheduled_time)
WHERE status = 'scheduled';
