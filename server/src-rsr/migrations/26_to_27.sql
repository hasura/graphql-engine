ALTER TABLE hdb_catalog.event_log
  ADD COLUMN archived BOOLEAN NOT NULL DEFAULT FALSE;

CREATE INDEX ON hdb_catalog.event_log (delivered);
