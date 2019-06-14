SELECT
  t.table_schema,
  t.table_name,
  t.table_oid,
  coalesce(c.columns, '[]') as columns,
  coalesce(f.constraints, '[]') as constraints,
  coalesce(fk.fkeys, '[]') as foreign_keys
FROM
  (
    SELECT
      c.oid as table_oid,
      c.relname as table_name,
      n.nspname as table_schema
    FROM
      pg_catalog.pg_class c
      JOIN pg_catalog.pg_namespace as n ON c.relnamespace = n.oid
  ) t
  LEFT OUTER JOIN (
    SELECT
      table_schema,
      table_name,
      json_agg(
        (
          SELECT
            r
          FROM
            (
              SELECT
                column_name,
                udt_name AS data_type,
                ordinal_position,
                is_nullable :: boolean
            ) r
        )
      ) as columns
    FROM
      information_schema.columns
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
WHERE
  t.table_schema NOT LIKE 'pg_%'
  AND t.table_schema <> 'information_schema'
  AND t.table_schema <> 'hdb_catalog'
