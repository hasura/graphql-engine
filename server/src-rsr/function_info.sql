SELECT
  row_to_json (
    (
      SELECT
        e
        FROM
            (
              SELECT
                has_variadic,
                function_type,
                return_type_schema,
                return_type_name,
                return_type_type,
                returns_set,
                input_arg_types,
                input_arg_names,
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
  ) AS "raw_function_info"
  FROM
      hdb_catalog.hdb_function_agg
 WHERE
  function_schema = $1
  AND function_name = $2
