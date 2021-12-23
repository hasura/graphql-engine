ALTER TABLE hdb_catalog.event_log DROP COLUMN archived;
DROP INDEX hdb_catalog.event_log_delivered_idx;
