ALTER TABLE hdb_catalog.event_log
  ADD COLUMN archived BOOLEAN NOT NULL DEFAULT FALSE;

UPDATE hdb_catalog.event_log
   SET archived = 't'
 WHERE
   trigger_name NOT IN (SELECT name from hdb_catalog.event_triggers);

CREATE INDEX ON hdb_catalog.event_log (delivered);
