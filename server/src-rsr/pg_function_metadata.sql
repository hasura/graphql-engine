SELECT
  "function_info".function_schema,
  "function_info".function_name,
  coalesce("function_info".info, '[]'::json) AS info
FROM (
  SELECT
    function_name,
    function_schema,
    -- This field corresponds to the 'RawFunctionInfo' Haskell type
    json_agg(
      json_build_object(
        'oid', "pg_function".function_oid,
        'description', "pg_function".description,
        'has_variadic', "pg_function".has_variadic,
        'function_type', "pg_function".function_type,
        'return_type_schema', "pg_function".return_type_schema,
        'return_type_name', "pg_function".return_type_name,
        'return_type_type', "pg_function".return_type_type,
        'returns_set', "pg_function".returns_set,
        'input_arg_types', "pg_function".input_arg_types,
        'input_arg_names', "pg_function".input_arg_names,
        'default_args', "pg_function".default_args,
        'returns_table', "pg_function".returns_table
      )
    ) AS info
    FROM (
      -- Necessary metadata from Postgres
      SELECT
        "function".function_name,
        "function".function_schema,
        pd.description,

        CASE
          WHEN ("function".provariadic = (0) :: oid) THEN false
          ELSE true
        END AS has_variadic,

        CASE
          WHEN (
            ("function".provolatile) :: text = ('i' :: character(1)) :: text
          ) THEN 'IMMUTABLE' :: text
          WHEN (
            ("function".provolatile) :: text = ('s' :: character(1)) :: text
          ) THEN 'STABLE' :: text
          WHEN (
            ("function".provolatile) :: text = ('v' :: character(1)) :: text
          ) THEN 'VOLATILE' :: text
          ELSE NULL :: text
        END AS function_type,

        pg_get_functiondef("function".function_oid) AS function_definition,

        rtn.nspname::text as return_type_schema,
        rt.typname::text as return_type_name,
        rt.typtype::text as return_type_type,
        "function".proretset AS returns_set,
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
                  COALESCE("function".proallargtypes, ("function".proargtypes) :: oid [])
                ) WITH ORDINALITY pat(oid, ordinality)
                LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
                LEFT JOIN pg_namespace pns ON (pt.typnamespace = pns.oid)
              ORDER BY pat.ordinality ASC
            ) q
         ) AS input_arg_types,
        to_json(COALESCE("function".proargnames, ARRAY [] :: text [])) AS input_arg_names,
        "function".pronargdefaults AS default_args,
        "function".function_oid::integer AS function_oid,
        (exists(
          SELECT
            1
            FROM
              information_schema.tables
            WHERE
              table_schema = rtn.nspname::text
              AND table_name = rt.typname::text
          ) OR
         exists(
           SELECT
             1
             FROM
                 pg_matviews
           WHERE
                schemaname = rtn.nspname::text
            AND matviewname = rt.typname::text
          )
        ) AS returns_table
      FROM
        jsonb_to_recordset($1::jsonb) AS tracked("schema" text, "name" text)
        JOIN
        ( SELECT   p.oid AS function_oid,
                   p.*,
                   p.proname::text AS function_name,
                   pn.nspname::text AS function_schema
              FROM pg_proc p
              JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
        ) "function" ON "function".function_name = tracked.name
                     AND "function".function_schema = tracked.schema
        JOIN pg_type rt ON (rt.oid = "function".prorettype)
        JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
        LEFT JOIN pg_description pd ON "function".function_oid = pd.objoid
      WHERE
        -- Do not fetch some default functions in public schema
        "function".function_name NOT LIKE 'pgp_%'
        AND "function".function_name NOT IN
                          ( 'armor'
                          , 'crypt'
                          , 'dearmor'
                          , 'decrypt'
                          , 'decrypt_iv'
                          , 'digest'
                          , 'encrypt'
                          , 'encrypt_iv'
                          , 'gen_random_bytes'
                          , 'gen_random_uuid'
                          , 'gen_salt'
                          , 'hmac'
                          )
        AND "function".function_schema NOT LIKE 'pg\_%'
        AND "function".function_schema NOT IN ('information_schema', 'hdb_catalog', '_timescaledb_internal')
        AND (NOT EXISTS (
                SELECT
                  1
                FROM
                  pg_aggregate
                WHERE
                  ((pg_aggregate.aggfnoid) :: oid = "function".function_oid)
              )
          )
    ) AS "pg_function"
    GROUP BY "pg_function".function_schema, "pg_function".function_name
  ) "function_info"
