SELECT
  t.table_schema,
  t.table_name,
  t.table_oid,
  t.description,
  coalesce(c.columns, '[]') as columns,
  coalesce(f.constraints, '[]') as constraints,
  coalesce(fk.fkeys, '[]') as foreign_keys,
  coalesce(cc.computed_fields, '[]') as computed_fields
FROM
  (
    SELECT
      c.oid as table_oid,
      c.relname as table_name,
      n.nspname as table_schema,
      pd.description as description
    FROM
      pg_catalog.pg_class c
      JOIN pg_catalog.pg_namespace as n ON c.relnamespace = n.oid
      LEFT JOIN pg_catalog.pg_description pd on (c.oid = pd.objoid and pd.objsubid = 0)
  ) t
  LEFT OUTER JOIN (
    SELECT
      table_schema,
      table_name,
      json_agg(
        json_build_object(
          'column_name', name,
          'data_type', type,
          'is_nullable', is_nullable :: boolean,
          'ordinal_position', ordinal_position,
          'references', primary_key_references,
          'description', description
        )
      ) as columns
    FROM
      hdb_catalog.hdb_column
    GROUP BY
      table_schema,
      table_name
  ) c ON (
    t.table_schema = c.table_schema
    AND t.table_name = c.table_name
  )
  LEFT OUTER JOIN (
    SELECT
      tc.table_schema,
      tc.table_name,
      json_agg(
        json_build_object(
          'name',
          tc.constraint_name,
          'oid',
          r.oid :: integer,
          'type',
          tc.constraint_type
        )
      ) as constraints
    FROM
      information_schema.table_constraints tc
      JOIN pg_catalog.pg_constraint r ON tc.constraint_name = r.conname
    GROUP BY
      table_schema,
      table_name
  ) f ON (
    t.table_schema = f.table_schema
    AND t.table_name = f.table_name
  )
  LEFT OUTER JOIN (
    SELECT
      f.table_schema,
      f.table_name,
      json_agg(
        json_build_object(
          'table',
          json_build_object(
            'schema',
            f.table_schema,
            'name',
            f.table_name
          ),
          'ref_table',
          json_build_object(
            'schema',
            f.ref_table_table_schema,
            'name',
            f.ref_table
          ),
          'oid',
          f.constraint_oid,
          'constraint',
          f.constraint_name,
          'column_mapping',
          f.column_mapping
        )
      ) as fkeys
    FROM
      hdb_catalog.hdb_foreign_key_constraint f
    GROUP BY
      table_schema,
      table_name
  ) fk ON (
    fk.table_schema = t.table_schema
    AND fk.table_name = t.table_name
  )
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
