ALTER TABLE hdb_catalog.event_log DROP COLUMN archived;
DROP INDEX hdb_catalog.event_log_delivered_idx;

UPDATE hdb_catalog.hdb_version
   SET version = '26'
 WHERE version = '27';

