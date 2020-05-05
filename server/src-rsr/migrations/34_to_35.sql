CREATE TABLE hdb_catalog.hdb_scheduled_trigger
(
  name TEXT PRIMARY KEY,
  webhook_conf JSON NOT NULL,
  schedule_conf JSON NOT NULL,
  payload JSON,
  retry_conf JSON,
  header_conf JSON,
  include_in_metadata BOOLEAN NOT NULL DEFAULT FALSE,
  comment TEXT
);

CREATE TABLE hdb_catalog.hdb_scheduled_events
(
id TEXT DEFAULT gen_random_uuid() UNIQUE,
name TEXT,
scheduled_time TIMESTAMPTZ NOT NULL,
additional_payload JSON,
status TEXT NOT NULL DEFAULT 'scheduled',
tries INTEGER NOT NULL DEFAULT 0,
created_at TIMESTAMP DEFAULT NOW(),
next_retry_at TIMESTAMPTZ,

PRIMARY KEY (name, scheduled_time),
FOREIGN KEY (name) REFERENCES hdb_catalog.hdb_scheduled_trigger(name)
ON UPDATE CASCADE ON DELETE CASCADE,
CONSTRAINT valid_status CHECK (status IN ('scheduled','locked','delivered','error','dead'))
);

CREATE INDEX hdb_scheduled_event_status ON hdb_catalog.hdb_scheduled_events (status);

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

CREATE VIEW hdb_catalog.hdb_scheduled_events_stats AS
  SELECT st.name,
         COALESCE(ste.upcoming_events_count,0) as upcoming_events_count,
         COALESCE(ste.max_scheduled_time, now()) as max_scheduled_time
  FROM hdb_catalog.hdb_scheduled_trigger st
  LEFT JOIN
  ( SELECT name, count(*) as upcoming_events_count, max(scheduled_time) as max_scheduled_time
    FROM hdb_catalog.hdb_scheduled_events
    WHERE tries = 0
    GROUP BY name
  ) ste
  ON st.name = ste.name;

CREATE TABLE hdb_catalog.hdb_one_off_scheduled_events
(
  id TEXT DEFAULT gen_random_uuid() UNIQUE,
  webhook_conf JSON NOT NULL,
  scheduled_time TIMESTAMPTZ NOT NULL,
  retry_conf JSON,
  payload JSON,
  status TEXT NOT NULL DEFAULT 'scheduled',
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),

  CONSTRAINT valid_status CHECK (status IN ('scheduled','locked','delivered','error','dead'))
);

CREATE INDEX hdb_hdb_one_off_scheduled_event_status ON hdb_catalog.hdb_one_off_scheduled_events (status);
