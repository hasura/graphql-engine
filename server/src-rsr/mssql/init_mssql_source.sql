CREATE TABLE hdb_catalog.hdb_source_catalog_version (
  version INTEGER NOT NULL PRIMARY KEY,
  upgraded_on DATETIME2(7) NOT NULL
);

CREATE TABLE hdb_catalog.event_log
(
  id UNIQUEIDENTIFIER DEFAULT newid() PRIMARY KEY,
  schema_name NVARCHAR(MAX) NOT NULL,
  table_name NVARCHAR(MAX) NOT NULL,
  /* The maximum key length for a nonclustered index is 1700 bytes. 
     Hence marking the 'n' for NVARCHAR as 850
   */
  trigger_name NVARCHAR(850) NOT NULL,
  payload NVARCHAR(MAX) NOT NULL,
  delivered BIT NOT NULL DEFAULT 0,
  error BIT NOT NULL DEFAULT 0,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at DATETIMEOFFSET(7) NOT NULL DEFAULT SYSDATETIMEOFFSET() AT TIME ZONE 'UTC',
  locked DATETIMEOFFSET(7),
  next_retry_at DATETIMEOFFSET(7),
  archived BIT NOT NULL DEFAULT 0
);

/* This powers `archiveEvents` */
CREATE INDEX event_log_archive_events ON hdb_catalog.event_log (trigger_name);

/* This index powers `fetchEvents` */
CREATE INDEX event_log_fetch_events
  ON hdb_catalog.event_log (locked asc, next_retry_at asc, created_at)
  WHERE delivered = 0
    AND error = 0
    AND archived = 0;

CREATE TABLE hdb_catalog.event_invocation_logs (
  id UNIQUEIDENTIFIER NOT NULL DEFAULT newid() PRIMARY KEY,
  trigger_name NVARCHAR(MAX),
  event_id UNIQUEIDENTIFIER,
  status INTEGER,
  request NVARCHAR(MAX),
  response NVARCHAR(MAX),
  created_at DATETIMEOFFSET(7) NOT NULL DEFAULT SYSDATETIMEOFFSET() AT TIME ZONE 'UTC'
);

/* This index improves the performance of deletes by event_id, so that if somebody
tries to delete an event from the hdb_catalog.event_log along with the invocation log
it will be faster with an index compared to without an index. */
CREATE INDEX fetch_event_invocation_logs ON hdb_catalog.event_invocation_logs (event_id);

CREATE TABLE hdb_catalog.hdb_event_log_cleanups
(
  id UNIQUEIDENTIFIER DEFAULT newid() PRIMARY KEY,
  trigger_name NVARCHAR(900) NOT NULL,
  scheduled_at DATETIMEOFFSET(7) NOT NULL,
  deleted_event_logs INTEGER,
  deleted_event_invocation_logs INTEGER,
  status NVARCHAR(MAX) NOT NULL,
  CHECK (status IN ('scheduled', 'paused', 'completed', 'dead')),

  UNIQUE (trigger_name, scheduled_at)
);
