SELECT
  row_to_json (
    (
      SELECT
        e
        FROM
            (
              SELECT
                hf.has_variadic,
                hf.function_type,
                hf.return_type_schema,
                hf.return_type_name,
                hf.return_type_type,
                hf.returns_set,
                hf.input_arg_types,
                hf.input_arg_names,
		COALESCE(pp.proallargtypes, pp.proargtypes::int[]) as arg_oids,
                exists(
                  SELECT
                    1
                    FROM
                        information_schema.tables
                   WHERE
                table_schema = hf.return_type_schema
                AND table_name = hf.return_type_name
                ) AS returns_table
            ) AS e
    )
  ) AS "raw_function_info"
  FROM
      hdb_catalog.hdb_function_agg hf left outer join pg_proc pp
      on hf.function_name = pp.proname and hf.function_schema = pp.pronamespace::regnamespace::text
 WHERE
  hf.function_schema = $1
  AND hf.function_name = $2
