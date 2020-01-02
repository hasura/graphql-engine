DROP FUNCTION IF EXISTS hdb_catalog.insert_event_log(text, text, text, text, json);

UPDATE hdb_catalog.hdb_version
  SET version = '14'
WHERE version = '15';


