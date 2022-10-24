CREATE TABLE hdb_catalog.hdb_event_log_cleanups
(
  id TEXT DEFAULT hdb_catalog.gen_hasura_uuid() PRIMARY KEY,
  trigger_name TEXT NOT NULL,
  scheduled_at TIMESTAMP NOT NULL,
  deleted_event_logs INTEGER,
  deleted_event_invocation_logs INTEGER,
  status TEXT NOT NULL,
  CHECK (status IN ('scheduled', 'paused', 'completed', 'dead')),

  UNIQUE (trigger_name, scheduled_at)
);

ALTER TABLE hdb_catalog.event_invocation_logs
ADD COLUMN trigger_name TEXT;

ALTER TABLE hdb_catalog.event_invocation_logs
DROP CONSTRAINT IF EXISTS event_invocation_logs_event_id_fkey;
