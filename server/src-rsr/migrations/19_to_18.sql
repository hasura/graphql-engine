DROP TRIGGER hdb_schema_update_event_notifier ON hdb_catalog.hdb_schema_update_event;

CREATE TRIGGER hdb_schema_update_event_notifier AFTER INSERT ON
  hdb_catalog.hdb_schema_update_event FOR EACH ROW EXECUTE PROCEDURE
                      hdb_catalog.hdb_schema_update_event_notifier();

DROP INDEX hdb_catalog."hdb_schema_update_event_one_row";

ALTER TABLE hdb_catalog.hdb_schema_update_event ADD COLUMN id SERIAL PRIMARY KEY;
