CREATE TABLE hdb_catalog.hdb_computed_field
  (
    table_schema TEXT,
    table_name TEXT,
    computed_field_name TEXT,
    definition JSONB NOT NULL,
    comment TEXT NULL,

PRIMARY KEY (table_schema, table_name, computed_field_name),
FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE
  );

CREATE VIEW hdb_catalog.hdb_computed_field_function AS
(
  SELECT
    table_schema,
    table_name,
    computed_field_name,
    CASE
      WHEN (definition::jsonb -> 'function')::jsonb ->> 'name' IS NULL THEN definition::jsonb ->> 'function'
      ELSE (definition::jsonb -> 'function')::jsonb ->> 'name'
    END AS function_name,
    CASE
      WHEN (definition::jsonb -> 'function')::jsonb ->> 'schema' IS NULL THEN 'public'
      ELSE (definition::jsonb -> 'function')::jsonb ->> 'schema'
    END AS function_schema
  FROM hdb_catalog.hdb_computed_field
);

CREATE OR REPLACE VIEW hdb_catalog.hdb_function_agg AS
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

  rtn.nspname::text as return_type_schema,
  rt.typname::text as return_type_name,
  rt.typtype::text as return_type_type,

  p.proretset AS returns_set,
  ( SELECT
      COALESCE(json_agg(
        json_build_object('schema', q."schema",
                          'name', q."name",
                          'type', q."type"
                         )
      ), '[]')
    FROM
      (
        SELECT
          pt.typname AS "name",
          pns.nspname AS "schema",
          pt.typtype AS "type",
          pat.ordinality
        FROM
          unnest(
            COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
          ) WITH ORDINALITY pat(oid, ordinality)
          LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
          LEFT JOIN pg_namespace pns ON (pt.typnamespace = pns.oid)
        ORDER BY pat.ordinality ASC
      ) q
   ) AS input_arg_types,
  to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names,
  p.pronargdefaults AS default_args,
  p.oid::integer AS function_oid
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
