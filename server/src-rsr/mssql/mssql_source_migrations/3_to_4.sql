-- created_at column previously had a default value in localtime which can result in timezone discrepancy, we are
-- changing it to UTC time.

-- Please note that we don't know the name of the default value constraint, that is why the query first fetches the name
-- from sys.default_constraints and then removes the default value constraint. Then we add a different default value to
-- the column created_at.
DECLARE @EventLogConstraintName nvarchar(200)
SELECT @EventLogConstraintName = name
    FROM sys.default_constraints
    WHERE parent_object_id = object_id('hdb_catalog.event_log')
        AND parent_column_id = columnproperty(object_id('hdb_catalog.event_log'), 'created_at', 'ColumnId')
IF @EventLogConstraintName IS NOT NULL
EXEC('ALTER TABLE hdb_catalog.event_log DROP CONSTRAINT ' + @EventLogConstraintName)

ALTER TABLE hdb_catalog.event_log ADD CONSTRAINT event_log_created_at_default_value DEFAULT SYSDATETIMEOFFSET() AT TIME ZONE 'UTC' FOR created_at; 

-- Please note that we don't know the name of the default value constraint, that is why the query first fetches the name
-- from sys.default_constraints and then removes the default value constraint. Then we add a different default value to
-- the column created_at.
DECLARE @EventInvocationLogConstraintName nvarchar(200)
SELECT @EventInvocationLogConstraintName = name
    FROM sys.default_constraints
    WHERE parent_object_id = object_id('hdb_catalog.event_invocation_logs')
        AND parent_column_id = columnproperty(object_id('hdb_catalog.event_invocation_logs'), 'created_at', 'ColumnId')
IF @EventInvocationLogConstraintName IS NOT NULL
EXEC('ALTER TABLE hdb_catalog.event_invocation_logs DROP CONSTRAINT ' + @EventInvocationLogConstraintName)

ALTER TABLE hdb_catalog.event_invocation_logs ADD CONSTRAINT event_invocation_logs_created_at_default_value DEFAULT SYSDATETIMEOFFSET() AT TIME ZONE 'UTC' FOR created_at; 
