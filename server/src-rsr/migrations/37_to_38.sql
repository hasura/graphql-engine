ALTER TABLE hdb_catalog.hdb_cron_events
ALTER COLUMN created_at TYPE TIMESTAMPTZ;

ALTER TABLE hdb_catalog.hdb_cron_event_invocation_logs
ALTER COLUMN created_at TYPE TIMESTAMPTZ;

ALTER TABLE hdb_catalog.hdb_scheduled_events
ALTER COLUMN created_at TYPE TIMESTAMPTZ;

ALTER TABLE hdb_catalog.hdb_scheduled_event_invocation_logs
ALTER COLUMN created_at TYPE TIMESTAMPTZ;
