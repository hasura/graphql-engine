-- This index is needed because when there's an ON CASCADE DELETE trigger in `hdb_catalog.hdb_cron_events`
-- which deletes the invocations along with the cron event. The `hdb_cron_event_invocation_logs` table
-- is joined using the `event_id` column and without an index on this column, the deletes take a really
-- long time.
CREATE INDEX hdb_cron_event_invocation_event_id ON hdb_catalog.hdb_cron_event_invocation_logs (event_id);
