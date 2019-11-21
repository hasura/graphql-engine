CREATE TABLE hdb_catalog.hdb_version (
    hasura_uuid UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    version TEXT NOT NULL,
    upgraded_on TIMESTAMPTZ NOT NULL,
    cli_state JSONB NOT NULL DEFAULT '{}'::jsonb,
    console_state JSONB NOT NULL DEFAULT '{}'::jsonb
);

CREATE UNIQUE INDEX hdb_version_one_row
ON hdb_catalog.hdb_version((version IS NOT NULL));

CREATE TABLE hdb_catalog.hdb_table
(
    table_schema TEXT,
    table_name TEXT,
    configuration JSONB,
    is_system_defined boolean default false,
    is_enum boolean NOT NULL DEFAULT false,

    PRIMARY KEY (table_schema, table_name)
);

CREATE TABLE hdb_catalog.hdb_relationship
(
    table_schema TEXT,
    table_name TEXT,
    rel_name   TEXT,
    rel_type   TEXT CHECK (rel_type IN ('object', 'array')),
    rel_def    JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name, rel_name),
    FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE
);

CREATE TABLE hdb_catalog.hdb_permission
(
    table_schema TEXT,
    table_name TEXT,
    role_name  TEXT,
    perm_type  TEXT CHECK(perm_type IN ('insert', 'select', 'update', 'delete')),
    perm_def   JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name, role_name, perm_type),
    FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE
);

CREATE VIEW hdb_catalog.hdb_permission_agg AS
SELECT
    table_schema,
    table_name,
    role_name,
    json_object_agg(perm_type, perm_def) as permissions
FROM
    hdb_catalog.hdb_permission
GROUP BY
    table_schema, table_name, role_name;

CREATE VIEW hdb_catalog.hdb_foreign_key_constraint AS
SELECT
    q.table_schema :: text,
    q.table_name :: text,
    q.constraint_name :: text,
    min(q.constraint_oid) :: integer as constraint_oid,
    min(q.ref_table_table_schema) :: text as ref_table_table_schema,
    min(q.ref_table) :: text as ref_table,
    json_object_agg(ac.attname, afc.attname) as column_mapping,
    min(q.confupdtype) :: text as on_update,
    min(q.confdeltype) :: text as on_delete,
    json_agg(ac.attname) as columns,
    json_agg(afc.attname) as ref_columns
FROM
    (SELECT
        ctn.nspname AS table_schema,
        ct.relname AS table_name,
        r.conrelid AS table_id,
        r.conname as constraint_name,
        r.oid as constraint_oid,
        cftn.nspname AS ref_table_table_schema,
        cft.relname as ref_table,
        r.confrelid as ref_table_id,
        r.confupdtype,
        r.confdeltype,
        UNNEST (r.conkey) AS column_id,
        UNNEST (r.confkey) AS ref_column_id
    FROM
        pg_catalog.pg_constraint r
        JOIN pg_catalog.pg_class ct
          ON r.conrelid = ct.oid
        JOIN pg_catalog.pg_namespace ctn
          ON ct.relnamespace = ctn.oid
        JOIN pg_catalog.pg_class cft
          ON r.confrelid = cft.oid
        JOIN pg_catalog.pg_namespace cftn
          ON cft.relnamespace = cftn.oid
    WHERE
        r.contype = 'f'
    ) q
    JOIN pg_catalog.pg_attribute ac
      ON q.column_id = ac.attnum
         AND q.table_id = ac.attrelid
    JOIN pg_catalog.pg_attribute afc
      ON q.ref_column_id = afc.attnum
         AND q.ref_table_id = afc.attrelid
GROUP BY q.table_schema, q.table_name, q.constraint_name;

CREATE VIEW hdb_catalog.hdb_check_constraint AS
SELECT
    n.nspname :: text AS table_schema,
    ct.relname :: text AS table_name,
    r.conname :: text as constraint_name,
    pg_catalog.pg_get_constraintdef(r.oid, true) as check
FROM
    pg_catalog.pg_constraint r
    JOIN pg_catalog.pg_class ct
      ON r.conrelid = ct.oid
    JOIN pg_catalog.pg_namespace n
      ON ct.relnamespace = n.oid
WHERE
    r.contype = 'c';

CREATE VIEW hdb_catalog.hdb_unique_constraint AS
SELECT
    tc.table_name,
    tc.constraint_schema AS table_schema,
    tc.constraint_name as constraint_name,
    json_agg(kcu.column_name) AS columns
FROM
    information_schema.table_constraints tc
    JOIN information_schema.key_column_usage AS kcu
    USING (constraint_schema, constraint_name)
WHERE
    constraint_type = 'UNIQUE'
GROUP BY
    tc.table_name, tc.constraint_schema, tc.constraint_name;

CREATE VIEW hdb_catalog.hdb_primary_key AS
SELECT
  tc.table_schema,
  tc.table_name,
  tc.constraint_name,
  json_agg(constraint_column_usage.column_name) AS columns
FROM
  (
    information_schema.table_constraints tc
    JOIN (
      SELECT
        x.tblschema AS table_schema,
        x.tblname AS table_name,
        x.colname AS column_name,
        x.cstrname AS constraint_name
      FROM
        (
          SELECT
            DISTINCT nr.nspname,
            r.relname,
            a.attname,
            c.conname
          FROM
            pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_depend d,
            pg_namespace nc,
            pg_constraint c
          WHERE
            (
              (nr.oid = r.relnamespace)
              AND (r.oid = a.attrelid)
              AND (d.refclassid = ('pg_class' :: regclass) :: oid)
              AND (d.refobjid = r.oid)
              AND (d.refobjsubid = a.attnum)
              AND (d.classid = ('pg_constraint' :: regclass) :: oid)
              AND (d.objid = c.oid)
              AND (c.connamespace = nc.oid)
              AND (c.contype = 'c' :: "char")
              AND (
                r.relkind = ANY (ARRAY ['r'::"char", 'p'::"char"])
              )
              AND (NOT a.attisdropped)
            )
          UNION ALL
          SELECT
            nr.nspname,
            r.relname,
            a.attname,
            c.conname
          FROM
            pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_namespace nc,
            pg_constraint c
          WHERE
            (
              (nr.oid = r.relnamespace)
              AND (r.oid = a.attrelid)
              AND (nc.oid = c.connamespace)
              AND (
                r.oid = CASE
                  c.contype
                  WHEN 'f' :: "char" THEN c.confrelid
                  ELSE c.conrelid
                END
              )
              AND (
                a.attnum = ANY (
                  CASE
                    c.contype
                    WHEN 'f' :: "char" THEN c.confkey
                    ELSE c.conkey
                  END
                )
              )
              AND (NOT a.attisdropped)
              AND (
                c.contype = ANY (ARRAY ['p'::"char", 'u'::"char", 'f'::"char"])
              )
              AND (
                r.relkind = ANY (ARRAY ['r'::"char", 'p'::"char"])
              )
            )
        ) x(
          tblschema,
          tblname,
          colname,
          cstrname
        )
    ) constraint_column_usage ON (
      (
        (tc.constraint_name) :: text = (constraint_column_usage.constraint_name) :: text
        AND (tc.table_schema) :: text = (constraint_column_usage.table_schema) :: text
        AND (tc.table_name) :: text = (constraint_column_usage.table_name) :: text
      )
    )
  )
WHERE
  ((tc.constraint_type) :: text = 'PRIMARY KEY' :: text)
GROUP BY
  tc.table_schema,
  tc.table_name,
  tc.constraint_name;

CREATE FUNCTION hdb_catalog.inject_table_defaults(view_schema text, view_name text, tab_schema text, tab_name text) RETURNS void
LANGUAGE plpgsql AS $$
    DECLARE
        r RECORD;
    BEGIN
      FOR r IN SELECT column_name, column_default FROM information_schema.columns WHERE table_schema = tab_schema AND table_name = tab_name AND column_default IS NOT NULL LOOP
          EXECUTE format('ALTER VIEW %I.%I ALTER COLUMN %I SET DEFAULT %s;', view_schema, view_name, r.column_name, r.column_default);
      END LOOP;
    END;
$$;


CREATE TABLE hdb_catalog.event_triggers
(
  name TEXT PRIMARY KEY,
  type TEXT NOT NULL,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  configuration JSON,
  comment TEXT,
  FOREIGN KEY (schema_name, table_name)
  REFERENCES hdb_catalog.hdb_table(table_schema, table_name) ON UPDATE CASCADE
);

CREATE TABLE hdb_catalog.event_log
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  trigger_name TEXT NOT NULL,
  payload JSONB NOT NULL,
  delivered BOOLEAN NOT NULL DEFAULT FALSE,
  error BOOLEAN NOT NULL DEFAULT FALSE,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  locked BOOLEAN NOT NULL DEFAULT FALSE,
  next_retry_at TIMESTAMP,
  archived BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE INDEX ON hdb_catalog.event_log (trigger_name);
CREATE INDEX ON hdb_catalog.event_log (locked);
CREATE INDEX ON hdb_catalog.event_log (delivered);

CREATE TABLE hdb_catalog.event_invocation_logs
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMP DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.event_log (id)
);

CREATE INDEX ON hdb_catalog.event_invocation_logs (event_id);

CREATE TABLE hdb_catalog.hdb_function
(
    function_schema TEXT,
    function_name TEXT,
    configuration JSONB NOT NULL DEFAULT '{}'::jsonb,
    is_system_defined boolean default false,

    PRIMARY KEY (function_schema, function_name)
);

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

CREATE TABLE hdb_catalog.remote_schemas (
  id BIGSERIAL PRIMARY KEY,
  name TEXT UNIQUE,
  definition JSON,
  comment TEXT
);

CREATE TABLE hdb_catalog.hdb_schema_update_event (
  instance_id uuid NOT NULL,
  occurred_at timestamptz NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX hdb_schema_update_event_one_row
  ON hdb_catalog.hdb_schema_update_event ((occurred_at IS NOT NULL));

CREATE FUNCTION hdb_catalog.hdb_schema_update_event_notifier() RETURNS trigger AS
$function$
  DECLARE
    instance_id uuid;
    occurred_at timestamptz;
    curr_rec record;
  BEGIN
    instance_id = NEW.instance_id;
    occurred_at = NEW.occurred_at;
    PERFORM pg_notify('hasura_schema_update', json_build_object(
      'instance_id', instance_id,
      'occurred_at', occurred_at
      )::text);
    RETURN curr_rec;
  END;
$function$
LANGUAGE plpgsql;

CREATE TRIGGER hdb_schema_update_event_notifier AFTER INSERT OR UPDATE ON
  hdb_catalog.hdb_schema_update_event FOR EACH ROW EXECUTE PROCEDURE
  hdb_catalog.hdb_schema_update_event_notifier();

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
        , col_description(pg_class.oid, columns.ordinal_position) AS description
     FROM information_schema.columns
JOIN pg_class ON pg_class.relname = columns.table_name
JOIN pg_namespace ON pg_namespace.oid = pg_class.relnamespace
     AND pg_namespace.nspname = columns.table_schema
LEFT JOIN primary_key_references AS pkey_refs
           ON columns.table_schema = pkey_refs.src_table_schema
          AND columns.table_name   = pkey_refs.src_table_name
          AND columns.column_name  = pkey_refs.src_column_name;

CREATE VIEW hdb_catalog.hdb_table_info_agg AS (
select
  tables.table_name as table_name,
  tables.table_schema as table_schema,
  descriptions.description,
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
          'references', primary_key_references,
          'description', description
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
  left outer join (
    select
        pc.relname as table_name,
        pn.nspname as table_schema,
        pd.description
    from pg_class pc
        left join pg_namespace pn on pn.oid = pc.relnamespace
        left join pg_description pd on pd.objoid = pc.oid
    where pd.objsubid = 0
  ) descriptions on (
    tables.table_schema = descriptions.table_schema
    AND tables.table_name = descriptions.table_name
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

CREATE OR REPLACE FUNCTION
  hdb_catalog.insert_event_log(schema_name text, table_name text, trigger_name text, op text, row_data json)
  RETURNS text AS $$
  DECLARE
    id text;
    payload json;
    session_variables json;
    server_version_num int;
  BEGIN
    id := gen_random_uuid();
    server_version_num := current_setting('server_version_num');
    IF server_version_num >= 90600 THEN
      session_variables := current_setting('hasura.user', 't');
    ELSE
      BEGIN
        session_variables := current_setting('hasura.user');
      EXCEPTION WHEN OTHERS THEN
                  session_variables := NULL;
      END;
    END IF;
    payload := json_build_object(
      'op', op,
      'data', row_data,
      'session_variables', session_variables
    );
    INSERT INTO hdb_catalog.event_log
                (id, schema_name, table_name, trigger_name, payload)
    VALUES
    (id, schema_name, table_name, trigger_name, payload);
    RETURN id;
  END;
$$ LANGUAGE plpgsql;

CREATE TABLE hdb_catalog.hdb_query_collection
(
  collection_name TEXT PRIMARY KEY,
  collection_defn JSONB NOT NULL,
  comment TEXT NULL,
  is_system_defined boolean default false
);

CREATE TABLE hdb_catalog.hdb_allowlist
(
  collection_name TEXT UNIQUE
    REFERENCES hdb_catalog.hdb_query_collection(collection_name)
);

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
