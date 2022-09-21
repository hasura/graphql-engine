-- event_id was previously NOT NULL, we are removing the constraint so that we can
-- delete the corresponding event logs while retaining the invocation logs.

ALTER TABLE hdb_catalog.event_invocation_logs
ALTER COLUMN event_id UNIQUEIDENTIFIER;

ALTER TABLE hdb_catalog.event_invocation_logs
ADD trigger_name NVARCHAR(MAX);

-- This will remove the foreign key constraint on the `event_invocation_logs`
-- table. Please note that we don't know the name of the foreign key constraint,
-- that is why the query first fetches the name from the INFORMATION_SCHEMA and
-- then removes the foreign key
DECLARE @ConstraintName nvarchar(200)
SELECT 
    @ConstraintName = CONSTRAINT_NAME
FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
where TABLE_NAME = 'event_invocation_logs'
AND CONSTRAINT_TYPE = 'FOREIGN KEY'
IF @ConstraintName IS NOT NULL
EXEC('alter table hdb_catalog.event_invocation_logs drop  CONSTRAINT ' + @ConstraintName)

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
