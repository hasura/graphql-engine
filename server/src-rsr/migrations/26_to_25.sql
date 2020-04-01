DROP VIEW hdb_catalog.hdb_function_info_agg;
DROP VIEW hdb_catalog.hdb_function_agg;

CREATE VIEW hdb_catalog.hdb_function_agg AS
(
SELECT
  p.proname::text AS function_name,
  pn.nspname::text AS function_schema,
  pd.description,

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
    WHEN ((rt.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSEUDO' :: text
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
  LEFT JOIN pg_description pd ON p.oid = pd.objoid
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

DROP VIEW hdb_catalog.hdb_computed_field_function;

DROP TABLE hdb_catalog.hdb_computed_field;
