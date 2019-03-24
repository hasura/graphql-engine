ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT event_triggers_pkey;

ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT event_triggers_name_key;

ALTER TABLE hdb_catalog.event_triggers
  ADD PRIMARY KEY (name);

ALTER TABLE hdb_catalog.event_triggers
  DROP COLUMN id;

ALTER TABLE hdb_catalog.event_log
  DROP COLUMN trigger_id;

CREATE INDEX ON hdb_catalog.event_log (trigger_name)
