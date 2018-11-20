CREATE TABLE hdb_catalog.hdb_function
(
    function_schema TEXT,
    function_name TEXT,
    is_system_defined boolean default false,

    PRIMARY KEY (function_schema, function_name)
);

CREATE VIEW hdb_catalog.hdb_function_agg AS
(
  SELECT
    r.routine_name AS function_name,
    r.routine_schema AS function_schema,
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
    r.type_udt_schema AS return_type_schema,
    r.type_udt_name AS return_type_name,
    CASE
      WHEN ((t.typtype) :: text = ('b' :: character(1)) :: text) THEN 'BASE' :: text
      WHEN ((t.typtype) :: text = ('c' :: character(1)) :: text) THEN 'COMPOSITE' :: text
      WHEN ((t.typtype) :: text = ('d' :: character(1)) :: text) THEN 'DOMAIN' :: text
      WHEN ((t.typtype) :: text = ('e' :: character(1)) :: text) THEN 'ENUM' :: text
      WHEN ((t.typtype) :: text = ('r' :: character(1)) :: text) THEN 'RANGE' :: text
      WHEN ((t.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSUEDO' :: text
      ELSE NULL :: text
    END AS return_type_type,
    p.proretset AS returns_set,
    to_json(
      (
        COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
      ) :: integer []
    ) AS input_arg_types,
    to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names
  FROM
    (
      (
        information_schema.routines r
        JOIN pg_proc p ON ((p.proname = (r.routine_name) :: name))
      )
      JOIN pg_type t ON ((t.oid = p.prorettype))
    )
  WHERE
    (
      ((r.routine_schema) :: text !~~ 'pg_%' :: text)
      AND (
        (r.routine_schema) :: text <> ALL (
          ARRAY ['information_schema'::text, 'hdb_catalog'::text, 'hdb_views'::text]
        )
      )
      AND (NOT EXISTS (SELECT 1 FROM pg_catalog.pg_aggregate WHERE aggfnoid = p.oid))
    )
  GROUP BY
    r.routine_name,
    r.routine_schema,
    p.oid,
    p.provariadic,
    p.provolatile,
    r.type_udt_schema,
    r.type_udt_name,
    t.typtype,
    p.proretset,
    p.proallargtypes,
    p.proargtypes,
    p.proargnames
);
