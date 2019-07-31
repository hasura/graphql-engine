-- Make hdb_catalog.hdb_schema_update_event as single row table
-- Delete insert trigger and setup update trigger

DELETE FROM hdb_catalog.hdb_schema_update_event;

CREATE UNIQUE INDEX hdb_schema_update_event_one_row
  ON hdb_catalog.hdb_schema_update_event ((occurred_at IS NOT NULL));

ALTER TABLE hdb_catalog.hdb_schema_update_event DROP COLUMN id;

DROP TRIGGER hdb_schema_update_event_notifier ON hdb_catalog.hdb_schema_update_event;

CREATE TRIGGER hdb_schema_update_event_notifier AFTER INSERT OR UPDATE ON
  hdb_catalog.hdb_schema_update_event FOR EACH ROW EXECUTE PROCEDURE
  hdb_catalog.hdb_schema_update_event_notifier();
