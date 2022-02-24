CREATE TABLE hdb_catalog.hdb_source_catalog_version (
  version INTEGER NOT NULL PRIMARY KEY,
  upgraded_on DATETIME2(7) NOT NULL
);

CREATE TABLE hdb_catalog.event_log
(
  id UNIQUEIDENTIFIER DEFAULT newid() PRIMARY KEY,
  schema_name NVARCHAR(MAX) NOT NULL,
  table_name NVARCHAR(MAX) NOT NULL,
  trigger_name NVARCHAR(MAX) NOT NULL,
  payload NVARCHAR(MAX) NOT NULL,
  delivered BIT NOT NULL DEFAULT 0,
  error BIT NOT NULL DEFAULT 0,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at DATETIMEOFFSET(7) NOT NULL DEFAULT SYSDATETIMEOFFSET(),
  locked DATETIMEOFFSET(7),
  next_retry_at DATETIMEOFFSET(7),
  archived BIT NOT NULL DEFAULT 0
);

/* This index powers `fetchEvents` */
CREATE INDEX event_log_fetch_events
  ON hdb_catalog.event_log (created_at)
  WHERE delivered = 0
    AND error = 0
    AND archived = 0;

CREATE TABLE hdb_catalog.event_invocation_logs (
  id UNIQUEIDENTIFIER NOT NULL DEFAULT newid(),
  event_id UNIQUEIDENTIFIER NOT NULL,
  status INTEGER,
  request NVARCHAR(MAX),
  response NVARCHAR(MAX),
  created_at DATETIMEOFFSET(7) NOT NULL DEFAULT SYSDATETIMEOFFSET(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.event_log(id)
);
