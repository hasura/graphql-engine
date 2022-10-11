ALTER TABLE hdb_catalog.event_log
ALTER COLUMN trigger_name NVARCHAR(850); 

/* This powers `archiveEvents` */
CREATE INDEX event_log_archive_events ON hdb_catalog.event_log (trigger_name);

DROP INDEX event_log_fetch_events ON hdb_catalog.event_log

/* This index powers `fetchEvents` */
CREATE INDEX event_log_fetch_events
  ON hdb_catalog.event_log (locked asc, next_retry_at asc, created_at)
  WHERE delivered = 0
    AND error = 0
    AND archived = 0;

ALTER TABLE hdb_catalog.event_invocation_logs ADD PRIMARY KEY (id);

/* This index improves the performance of deletes by event_id, so that if somebody
tries to delete an event from the hdb_catalog.event_log along with the invocation log
it will be faster with an index compared to without an index. */
CREATE INDEX fetch_event_invocation_logs ON hdb_catalog.event_invocation_logs (event_id);
