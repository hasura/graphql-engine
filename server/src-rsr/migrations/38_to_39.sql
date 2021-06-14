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
