ALTER TABLE hdb_catalog.event_triggers
  ADD COLUMN headers JSON;

ALTER TABLE hdb_catalog.event_log
  ADD COLUMN next_retry_at TIMESTAMP;

CREATE INDEX ON hdb_catalog.event_log (trigger_id);

CREATE INDEX ON hdb_catalog.event_invocation_logs (event_id);
