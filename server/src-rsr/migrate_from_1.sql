-- required for generating uuid
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE hdb_catalog.event_triggers
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  name TEXT UNIQUE,
  type TEXT NOT NULL,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  definition JSON,
  query TEXT,
  webhook TEXT NOT NULL,
  num_retries INTEGER DEFAULT 0,
  retry_interval INTEGER DEFAULT 10,
  comment TEXT
);

CREATE TABLE hdb_catalog.event_log
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  trigger_id TEXT NOT NULL,
  trigger_name TEXT NOT NULL,
  payload JSONB NOT NULL,
  delivered BOOLEAN NOT NULL DEFAULT FALSE,
  error BOOLEAN NOT NULL DEFAULT FALSE,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  locked BOOLEAN NOT NULL DEFAULT FALSE
);

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
