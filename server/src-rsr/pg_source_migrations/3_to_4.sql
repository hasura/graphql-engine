ALTER table hdb_catalog.event_log
  ALTER COLUMN created_at SET DEFAULT NOW() AT TIME ZONE 'utc';

ALTER table hdb_catalog.event_invocation_logs
  ALTER COLUMN created_at SET DEFAULT NOW() AT TIME ZONE 'utc';
