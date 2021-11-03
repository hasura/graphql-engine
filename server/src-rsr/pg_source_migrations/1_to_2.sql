DROP INDEX hdb_catalog.event_log_delivered_idx;
DROP INDEX hdb_catalog.event_log_created_at_idx;

/* This index powers `fetchEvents` */
CREATE INDEX event_log_fetch_events
  ON hdb_catalog.event_log (locked NULLS FIRST, next_retry_at NULLS FIRST, created_at)
  WHERE delivered = 'f'
    and error = 'f'
    and archived = 'f'
;
