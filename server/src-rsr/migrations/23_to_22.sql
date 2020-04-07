DROP VIEW IF EXISTS hdb_catalog.hdb_table_info_agg;
DROP VIEW IF EXISTS hdb_catalog.hdb_column;

CREATE VIEW hdb_catalog.hdb_column AS
     WITH primary_key_references AS (
            SELECT fkey.table_schema           AS src_table_schema
                 , fkey.table_name             AS src_table_name
                 , fkey.columns->>0            AS src_column_name
                 , json_agg(json_build_object(
                     'schema', fkey.ref_table_table_schema,
                     'name', fkey.ref_table
                   )) AS ref_tables
              FROM hdb_catalog.hdb_foreign_key_constraint AS fkey
              JOIN hdb_catalog.hdb_primary_key            AS pkey
                    ON pkey.table_schema   = fkey.ref_table_table_schema
                   AND pkey.table_name     = fkey.ref_table
                   AND pkey.columns::jsonb = fkey.ref_columns::jsonb
             WHERE json_array_length(fkey.columns) = 1
          GROUP BY fkey.table_schema
                 , fkey.table_name
                 , fkey.columns->>0)
   SELECT columns.table_schema
        , columns.table_name
        , columns.column_name AS name
        , columns.udt_name AS type
        , columns.is_nullable
        , columns.ordinal_position
        , coalesce(pkey_refs.ref_tables, '[]') AS primary_key_references
     FROM information_schema.columns
LEFT JOIN primary_key_references AS pkey_refs
           ON columns.table_schema = pkey_refs.src_table_schema
          AND columns.table_name   = pkey_refs.src_table_name
          AND columns.column_name  = pkey_refs.src_column_name;

CREATE VIEW hdb_catalog.hdb_table_info_agg AS (
select
  tables.table_name as table_name,
  tables.table_schema as table_schema,
  coalesce(columns.columns, '[]') as columns,
  coalesce(pk.columns, '[]') as primary_key_columns,
  coalesce(constraints.constraints, '[]') as constraints,
  coalesce(views.view_info, 'null') as view_info
from
  information_schema.tables as tables
  left outer join (
    select
      c.table_name,
      c.table_schema,
      json_agg(
        json_build_object(
          'name', name,
          'type', type,
          'is_nullable', is_nullable :: boolean,
          'references', primary_key_references
        )
      ) as columns
    from
      hdb_catalog.hdb_column c
    group by
      c.table_schema,
      c.table_name
  ) columns on (
    tables.table_schema = columns.table_schema
    AND tables.table_name = columns.table_name
  )
  left outer join (
    select * from hdb_catalog.hdb_primary_key
  ) pk on (
    tables.table_schema = pk.table_schema
    AND tables.table_name = pk.table_name
  )
  left outer join (
    select
      c.table_schema,
      c.table_name,
      json_agg(constraint_name) as constraints
    from
      information_schema.table_constraints c
    where
      c.constraint_type = 'UNIQUE'
      or c.constraint_type = 'PRIMARY KEY'
    group by
      c.table_schema,
      c.table_name
  ) constraints on (
    tables.table_schema = constraints.table_schema
    AND tables.table_name = constraints.table_name
  )
  left outer join (
    select
      table_schema,
      table_name,
      json_build_object(
        'is_updatable',
        (is_updatable::boolean OR is_trigger_updatable::boolean),
        'is_deletable',
        (is_updatable::boolean OR is_trigger_deletable::boolean),
        'is_insertable',
        (is_insertable_into::boolean OR is_trigger_insertable_into::boolean)
      ) as view_info
    from
      information_schema.views v
  ) views on (
    tables.table_schema = views.table_schema
    AND tables.table_name = views.table_name
  )
);

DROP VIEW IF EXISTS hdb_catalog.hdb_function_info_agg;
DROP VIEW IF EXISTS hdb_catalog.hdb_function_agg;

CREATE VIEW hdb_catalog.hdb_function_agg AS
(
SELECT
  p.proname::text AS function_name,
  pn.nspname::text AS function_schema,

  CASE
    WHEN (p.provariadic = (0) :: oid) THEN false
    ELSE true
  END AS has_variadic,

  CASE
    WHEN (
      (p.provolatile) :: text = ('i' :: character(1)) :: text
    ) THEN 'IMMUTABLE' :: text
    WHEN (
      (p.provolatile) :: text = ('s' :: character(1)) :: text
    ) THEN 'STABLE' :: text
    WHEN (
      (p.provolatile) :: text = ('v' :: character(1)) :: text
    ) THEN 'VOLATILE' :: text
    ELSE NULL :: text
  END AS function_type,

  pg_get_functiondef(p.oid) AS function_definition,

  rtn.nspname::text AS return_type_schema,
  rt.typname::text AS return_type_name,

  CASE
    WHEN ((rt.typtype) :: text = ('b' :: character(1)) :: text) THEN 'BASE' :: text
    WHEN ((rt.typtype) :: text = ('c' :: character(1)) :: text) THEN 'COMPOSITE' :: text
    WHEN ((rt.typtype) :: text = ('d' :: character(1)) :: text) THEN 'DOMAIN' :: text
    WHEN ((rt.typtype) :: text = ('e' :: character(1)) :: text) THEN 'ENUM' :: text
    WHEN ((rt.typtype) :: text = ('r' :: character(1)) :: text) THEN 'RANGE' :: text
    WHEN ((rt.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSUEDO' :: text
    ELSE NULL :: text
  END AS return_type_type,
  p.proretset AS returns_set,
  ( SELECT
      COALESCE(json_agg(q.type_name), '[]')
    FROM
      (
        SELECT
          pt.typname AS type_name,
          pat.ordinality
        FROM
          unnest(
            COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
          ) WITH ORDINALITY pat(oid, ordinality)
          LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
        ORDER BY pat.ordinality ASC
      ) q
   ) AS input_arg_types,
  to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names,
  p.pronargdefaults AS default_args
FROM
  pg_proc p
  JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
  JOIN pg_type rt ON (rt.oid = p.prorettype)
  JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
WHERE
  pn.nspname :: text NOT LIKE 'pg_%'
  AND pn.nspname :: text NOT IN ('information_schema', 'hdb_catalog', 'hdb_views')
  AND (NOT EXISTS (
          SELECT
            1
          FROM
            pg_aggregate
          WHERE
            ((pg_aggregate.aggfnoid) :: oid = p.oid)
        )
    )
);

CREATE VIEW hdb_catalog.hdb_function_info_agg AS (
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
