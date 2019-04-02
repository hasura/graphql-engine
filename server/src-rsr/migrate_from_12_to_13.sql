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
  rt.typtype::text AS return_type_type,

  p.proretset AS returns_set,
  (
    SELECT
      COALESCE(
        json_agg(
          json_build_object('schema', q."schema", 'name', q."name", 'type', q."type")
        ),
        '[]' :: json
      )
      FROM
          (
            SELECT
              pt.typname AS "name",
              pns.nspname AS "schema",
              pt.typtype AS "type",
              pat.ordinality
              from
                  unnest(COALESCE(p.proallargtypes, p.proargtypes)) WITH ORDINALITY pat(oid, ordinality)
                  LEFT JOIN pg_catalog.pg_type pt on (pt.oid = pat.oid)
                  LEFT JOIN pg_catalog.pg_namespace pns ON (pt.typnamespace = pns.oid)
             ORDER BY
          pat.ordinality ASC
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

ALTER TABLE hdb_catalog.hdb_function ADD COLUMN function_type TEXT NOT NULL DEFAULT 'QUERY';

CREATE TABLE hdb_catalog.hdb_computed_column
(
  function_schema TEXT,
  function_name TEXT,
  table_schema TEXT,
  table_name TEXT,

  PRIMARY KEY (function_schema, function_name),
  FOREIGN KEY (function_schema, function_name) REFERENCES
    hdb_catalog.hdb_function (function_schema, function_name) ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (table_schema, table_name) REFERENCES
    hdb_catalog.hdb_table (table_schema, table_name) ON UPDATE CASCADE ON DELETE CASCADE
);
