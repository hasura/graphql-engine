-- This migration enables metadata separation
-- Drops all tables which stores hasura metadata
DROP VIEW hdb_catalog.hdb_role;
DROP TABLE hdb_catalog.hdb_custom_types;
DROP TABLE hdb_catalog.hdb_action_permission;
DROP TABLE hdb_catalog.hdb_action;
DROP VIEW hdb_catalog.hdb_computed_field_function;
DROP TABLE hdb_catalog.hdb_computed_field;
DROP TABLE hdb_catalog.hdb_allowlist;
DROP TABLE hdb_catalog.hdb_query_collection;
DROP VIEW hdb_catalog.hdb_function_info_agg;
DROP VIEW hdb_catalog.hdb_table_info_agg;
DROP TRIGGER hdb_schema_update_event_notifier ON hdb_catalog.hdb_schema_update_event;
DROP FUNCTION hdb_catalog.hdb_schema_update_event_notifier();
DROP TABLE hdb_catalog.hdb_schema_update_event; -- https://github.com/hasura/graphql-engine/pull/6173
DROP TABLE hdb_catalog.remote_schemas;
DROP VIEW hdb_catalog.hdb_function_agg;
DROP TABLE hdb_catalog.hdb_function;
DROP TABLE hdb_catalog.event_triggers;
DROP FUNCTION hdb_catalog.inject_table_defaults(text, text, text, text);
DROP VIEW hdb_catalog.hdb_primary_key;
DROP VIEW hdb_catalog.hdb_unique_constraint;
DROP VIEW hdb_catalog.hdb_check_constraint;
DROP VIEW hdb_catalog.hdb_foreign_key_constraint;
DROP VIEW hdb_catalog.hdb_permission_agg;
DROP TABLE hdb_catalog.hdb_permission;
DROP TABLE hdb_catalog.hdb_remote_relationship;
DROP TABLE hdb_catalog.hdb_relationship;
DROP TRIGGER event_trigger_table_name_update_trigger ON hdb_catalog.hdb_table;
DROP FUNCTION hdb_catalog.event_trigger_table_name_update();
DROP TABLE hdb_catalog.hdb_table;
DROP FUNCTION hdb_catalog.check_violation(text);

-- Remove foreign key constraint to hdb_cron_triggers in hdb_cron_events
ALTER TABLE hdb_catalog.hdb_cron_events DROP CONSTRAINT hdb_cron_events_trigger_name_fkey;

-- Now drop hdb_cron_triggers
DROP VIEW hdb_catalog.hdb_cron_events_stats;
DROP TABLE hdb_catalog.hdb_cron_triggers;

-- Create table which stores metadata JSON blob
CREATE TABLE hdb_catalog.hdb_metadata
(
  id INTEGER PRIMARY KEY,
  metadata JSON NOT NULL
);

-- DROP hdb_views schema (https://github.com/hasura/graphql-engine/pull/6135)
DROP SCHEMA IF EXISTS hdb_views CASCADE;
