ALTER TABLE hdb_catalog.hdb_cron_events
ALTER COLUMN created_at TYPE TIMESTAMP;

ALTER TABLE hdb_catalog.hdb_cron_event_invocation_logs
ALTER COLUMN created_at TYPE TIMESTAMP;

ALTER TABLE hdb_catalog.hdb_scheduled_events
ALTER COLUMN created_at TYPE TIMESTAMP;

ALTER TABLE hdb_catalog.hdb_scheduled_event_invocation_logs
ALTER COLUMN created_at TYPE TIMESTAMP;

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
                            ) AS returns_table
                    ) AS e
            )
        ) AS "function_info"
    FROM
        hdb_catalog.hdb_function_agg
);

DROP TRIGGER event_trigger_table_name_update_trigger ON hdb_catalog.hdb_table;

DROP FUNCTION hdb_catalog.event_trigger_table_name_update();

ALTER TABLE hdb_catalog.event_triggers
ADD CONSTRAINT event_triggers_schema_name_table_name_fkey
FOREIGN KEY (schema_name, table_name)
REFERENCES hdb_catalog.hdb_table(table_schema, table_name)
ON UPDATE CASCADE;
