CREATE TABLE hdb_catalog.hdb_scheduled_trigger
(
  name TEXT PRIMARY KEY,
  webhook_conf JSON NOT NULL,
  schedule JSON NOT NULL,
  payload JSON,
  retry_conf JSON
);

CREATE TABLE hdb_catalog.hdb_scheduled_events
(
  id TEXT DEFAULT gen_random_uuid() UNIQUE,
  name TEXT,
  scheduled_time TIMESTAMP NOT NULL,
  delivered BOOLEAN NOT NULL DEFAULT FALSE,
  error BOOLEAN NOT NULL DEFAULT FALSE,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  locked BOOLEAN NOT NULL DEFAULT FALSE,
  dead BOOLEAN NOT NULL DEFAULT FALSE,
  next_retry_at TIMESTAMP,

  PRIMARY KEY (name, scheduled_time),
  FOREIGN KEY (name) REFERENCES hdb_catalog.hdb_scheduled_trigger(name)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE hdb_catalog.hdb_scheduled_event_invocation_logs
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMP DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.hdb_scheduled_events (id) ON DELETE CASCADE
);
