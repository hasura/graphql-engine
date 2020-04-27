DROP VIEW hdb_catalog.hdb_scheduled_events_stats;
DROP TABLE hdb_catalog.hdb_scheduled_event_invocation_logs;
DROP TABLE hdb_catalog.hdb_scheduled_events;
DROP TABLE hdb_catalog.hdb_scheduled_trigger;

DELETE FROM hdb_catalog.hdb_relationship
where table_schema = 'hdb_catalog' and
table_name in ('hdb_scheduled_event_invocation_logs','hdb_scheduled_events','hdb_scheduled_trigger');

DELETE FROM hdb_catalog.hdb_table
where table_schema = 'hdb_catalog' and
table_name in ('hdb_scheduled_event_invocation_logs','hdb_scheduled_events','hdb_scheduled_trigger');
