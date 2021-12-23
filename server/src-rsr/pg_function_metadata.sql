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
        p.oid::integer AS function_oid,
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
        pg_proc p
        JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
        JOIN pg_type rt ON (rt.oid = p.prorettype)
        JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
        LEFT JOIN pg_description pd ON p.oid = pd.objoid
      WHERE
        -- Do not fetch some default functions in public schema
        p.proname :: text NOT LIKE 'pgp_%'
        AND p.proname :: text NOT IN
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
        AND pn.nspname :: text NOT LIKE 'pg_%'
        AND pn.nspname :: text NOT IN ('information_schema', 'hdb_catalog')
        AND (NOT EXISTS (
                SELECT
                  1
                FROM
                  pg_aggregate
                WHERE
                  ((pg_aggregate.aggfnoid) :: oid = p.oid)
              )
          )
    ) AS "pg_function"
    GROUP BY "pg_function".function_schema, "pg_function".function_name
  ) "function_info"
