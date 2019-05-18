CREATE OR REPLACE VIEW hdb_catalog.hdb_function_agg AS
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
      COALESCE(json_agg(q.type_info), '[]')
    FROM
      (
        SELECT
          json_build_object(
            'oid', pat.oid::int,
            'dimension',
            case
              when pt.oid IS NOT NULL then 1
              else 0
            end,
            'name', format_type(pat.oid, null)
          ) AS type_info,
          pat.ordinality
        FROM
          UNNEST(
              COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
          ) WITH ORDINALITY pat(oid, ordinality)
          LEFT OUTER JOIN
            pg_type pt ON (pt.typarray = pat.oid)
        ORDER BY pat.ordinality ASC
      ) q
   ) AS input_arg_types,
  to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names
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

CREATE OR REPLACE VIEW hdb_catalog.hdb_table_info_agg AS (
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
          'name',
          column_name,
          'type',
          json_build_object(
            'oid',
            ty.oid :: int ,
            'dimension',
            td.attndims
          ),
          'is_nullable',
          is_nullable :: boolean
        )
      ) as columns
    from
      information_schema.columns c
      left outer join (
        select pc.relnamespace,
               pc.relname,
               pa.attname,
               pa.attndims,
               pa.atttypid,
               pa.atttypmod
          from pg_attribute pa
                 left join pg_class pc
                     on pa.attrelid = pc.oid
      ) td on
      ( c.table_schema::regnamespace::oid = td.relnamespace
      AND c.table_name = td.relname
      AND c.column_name = td.attname
      )
      left outer join pg_type ty
                     on td.atttypid = ty.oid
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

CREATE OR REPLACE VIEW hdb_catalog.hdb_type_info AS
(
select
  t.oid :: integer as oid,
  json_build_object(
    'name',
    t.typname,
    'schema',
    ns.nspname
  ) as name,
  pg_catalog.format_type(t.oid, NULL) as sql_name,
  case
    when arr_elem.oid is not null then json_build_object(
      'type',
      'array',
      'elem_oid',
      arr_elem.oid :: integer
    )
    when t.typtype = 'b' then json_build_object('type', 'base')
    when t.typtype = 'e' then json_build_object(
      'type',
      'enum',
      'possible_values',
      (
        select
          array_agg(
            enumlabel
            order by
              enumsortorder
          )
        from
          pg_enum
        where
          enumtypid = t.oid
      )
    )
    when t.typtype = 'c' then json_build_object(
      'type',
      'composite',
      'fields',
      (
        select
          json_agg(
            (
              select
                json_build_object(
                  attname,
                  (
                    select
                      row_to_json(x)
                    from
                      (
                        select
                          atttypid :: integer as oid,
                          attndims as dimension
                      ) x
                  )
                )
            )
          )
        from
          pg_attribute
        where
          attrelid = t.typrelid
      )
    )
    when t.typtype = 'd' then json_build_object(
      'type',
      'domain',
      'base_type',
      json_build_object(
        'oid',
        t.typbasetype :: integer,
        'dimension',
        t.typndims
      )
    )
    when t.typtype = 'p' then json_build_object('type', 'pseudo')
    when t.typtype = 'r' then json_build_object('type', 'range')
    else null
  end as detail
from
  pg_type t
  left outer join pg_namespace ns on t.typnamespace = ns.oid
  left outer join pg_type arr_elem on t.oid = arr_elem.typarray
);
