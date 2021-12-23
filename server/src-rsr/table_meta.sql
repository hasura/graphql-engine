SELECT
  t.table_schema,
  t.table_name,
  t.info :: json,
  coalesce(cc.computed_fields, '[]') as computed_fields
FROM hdb_catalog.hdb_table_info_agg t
  LEFT OUTER JOIN (
    SELECT
      c.table_schema,
      c.table_name,
      json_agg(
        json_build_object(
          'name', c.computed_field_name,
          'function_meta',
          json_build_object(
            'function', json_build_object('name', c.function_name, 'schema', c.function_schema),
            'oid', hf_agg.function_oid,
            'type', hf_agg.function_type,
            'description', hf_agg.description
          )
        )
      ) as computed_fields
     FROM hdb_catalog.hdb_function_agg hf_agg
     LEFT OUTER JOIN  hdb_catalog.hdb_computed_field_function c
       ON ( hf_agg.function_name = c.function_name
           AND hf_agg.function_schema = c.function_schema
          )
     GROUP BY
       c.table_schema,
       c.table_name
  ) cc ON (
    cc.table_schema = t.table_schema
    AND cc.table_name = t.table_name
  )
WHERE
  t.table_schema NOT LIKE 'pg_%'
  AND t.table_schema <> 'information_schema'
  AND t.table_schema <> 'hdb_catalog'
  AND t.table_schema <> 'hdb_views'
