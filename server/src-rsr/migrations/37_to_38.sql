ALTER TABLE hdb_catalog.hdb_cron_events
ALTER COLUMN created_at TYPE TIMESTAMPTZ;

ALTER TABLE hdb_catalog.hdb_cron_event_invocation_logs
ALTER COLUMN created_at TYPE TIMESTAMPTZ;

ALTER TABLE hdb_catalog.hdb_scheduled_events
ALTER COLUMN created_at TYPE TIMESTAMPTZ;

ALTER TABLE hdb_catalog.hdb_scheduled_event_invocation_logs
ALTER COLUMN created_at TYPE TIMESTAMPTZ;

CREATE
OR REPLACE VIEW hdb_catalog.hdb_function_info_agg AS (
    SELECT
        function_name,
        function_schema,
        row_to_json (
            (
                SELECT
                    e
                FROM
                    (
                        SELECT
                            description,
                            has_variadic,
                            function_type,
                            return_type_schema,
                            return_type_name,
                            return_type_type,
                            returns_set,
                            input_arg_types,
                            input_arg_names,
                            default_args,
                            exists(
                                SELECT
                                    1
                                FROM
                                    information_schema.tables
                                WHERE
                                    table_schema = return_type_schema
                                    AND table_name = return_type_name
                            )
                            OR exists(
                                SELECT
                                    1
                                FROM
                                    pg_matviews
                                WHERE
                                    schemaname = return_type_schema
                                    AND matviewname = return_type_name
                            ) AS returns_table
                    ) AS e
            )
        ) AS "function_info"
    FROM
        hdb_catalog.hdb_function_agg
);

ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT IF EXISTS event_triggers_schema_name_fkey;

ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT IF EXISTS event_triggers_schema_name_table_name_fkey;

-- since we removed the foreign key constraint with hdb_catalog.hdb_table which had 'ON UPDATE CASCADE'
-- (see Note [Diff-and-patch event triggers on replace] in Hasura.RQL.DDL.EventTrigger), we perform the update using trigger
CREATE OR REPLACE FUNCTION hdb_catalog.event_trigger_table_name_update()
RETURNS TRIGGER
LANGUAGE PLPGSQL
AS
$$
BEGIN
  IF (NEW.table_schema, NEW.table_name) <> (OLD.table_schema, OLD.table_name)  THEN
    UPDATE hdb_catalog.event_triggers
    SET schema_name = NEW.table_schema, table_name = NEW.table_name
    WHERE (schema_name, table_name) = (OLD.table_schema, OLD.table_name);
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER event_trigger_table_name_update_trigger
AFTER UPDATE ON hdb_catalog.hdb_table
FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.event_trigger_table_name_update();
