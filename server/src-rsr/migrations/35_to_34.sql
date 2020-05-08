DROP TABLE hdb_catalog.hdb_scheduled_event_invocation_logs;
DROP TABLE hdb_catalog.hdb_scheduled_events;
DROP VIEW hdb_catalog.hdb_cron_events_stats;
DROP TABLE hdb_catalog.hdb_cron_event_invocation_logs;
DROP TABLE hdb_catalog.hdb_cron_events;
DROP TABLE hdb_catalog.hdb_cron_triggers;

DELETE FROM hdb_catalog.hdb_relationship
where table_schema = 'hdb_catalog' and
table_name in
('hdb_scheduled_event_invocation_logs','hdb_scheduled_events','hdb_cron_event_invocation_logs','hdb_cron_events'
,'hdb_catalog.hdb_cron_triggers');

DELETE FROM hdb_catalog.hdb_table
where table_schema = 'hdb_catalog' and
table_name in
('hdb_scheduled_event_invocation_logs','hdb_scheduled_events','hdb_cron_event_invocation_logs','hdb_cron_events'
,'hdb_catalog.hdb_cron_triggers');
