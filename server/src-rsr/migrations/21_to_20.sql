DROP INDEX hdb_catalog."event_log_locked_idx";

UPDATE hdb_catalog.hdb_version
  SET version = '20'
WHERE version = '21';

