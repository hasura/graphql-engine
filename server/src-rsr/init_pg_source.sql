CREATE TABLE hdb_catalog.hdb_source_catalog_version(
  version TEXT NOT NULL,
  upgraded_on TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX hdb_source_catalog_version_one_row
ON hdb_catalog.hdb_source_catalog_version((version IS NOT NULL));

CREATE TABLE hdb_catalog.event_log
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  trigger_name TEXT NOT NULL,
  payload JSONB NOT NULL,
  delivered BOOLEAN NOT NULL DEFAULT FALSE,
  error BOOLEAN NOT NULL DEFAULT FALSE,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  locked BOOLEAN NOT NULL DEFAULT FALSE,
  next_retry_at TIMESTAMP,
  archived BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE INDEX ON hdb_catalog.event_log (trigger_name);
CREATE INDEX ON hdb_catalog.event_log (locked);
CREATE INDEX ON hdb_catalog.event_log (delivered);
CREATE INDEX ON hdb_catalog.event_log (created_at);

CREATE TABLE hdb_catalog.event_invocation_logs
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMP DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.event_log (id)
);

CREATE INDEX ON hdb_catalog.event_invocation_logs (event_id);
