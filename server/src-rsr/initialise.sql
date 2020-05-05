CREATE TABLE hdb_catalog.hdb_version (
    hasura_uuid UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    version TEXT NOT NULL,
    upgraded_on TIMESTAMPTZ NOT NULL,
    cli_state JSONB NOT NULL DEFAULT '{}'::jsonb,
    console_state JSONB NOT NULL DEFAULT '{}'::jsonb
);

CREATE UNIQUE INDEX hdb_version_one_row
ON hdb_catalog.hdb_version((version IS NOT NULL));

/* Note [Reference system columns using type name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While working on #3394, I (Alexis) discovered that Postgres seems to sometimes generate very bad
query plans when joining against the system catalogs if we store things like table/schema names
using type `text` rather than type `name`, the latter of which is used internally. The two types are
compatible in the sense that Postgres will willingly widen `name` to `type` automatically, but
`name`s are restricted to 64 bytes.

Using `name` for ordinary user data would be a deep sin, but using it to store references to actual
Postgres identifiers makes a lot of sense, so using `name` in those places is alright. And by doing
so, we make Postgres much more likely to take advantage of certain indexes that can significantly
improve query performance. */

CREATE TABLE hdb_catalog.hdb_table
(
    table_schema name, -- See Note [Reference system columns using type name]
    table_name name,
    configuration jsonb,
    is_system_defined boolean default false,
    is_enum boolean NOT NULL DEFAULT false,

    PRIMARY KEY (table_schema, table_name)
);

CREATE TABLE hdb_catalog.hdb_relationship
(
    table_schema name, -- See Note [Reference system columns using type name]
    table_name name,
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
    table_schema name, -- See Note [Reference system columns using type name]
    table_name name,
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
CREATE INDEX ON hdb_catalog.event_log (created_at);

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
  occurred_at timestamptz NOT NULL DEFAULT NOW(),
  invalidations json NOT NULL
);

CREATE UNIQUE INDEX hdb_schema_update_event_one_row
  ON hdb_catalog.hdb_schema_update_event ((occurred_at IS NOT NULL));

CREATE FUNCTION hdb_catalog.hdb_schema_update_event_notifier() RETURNS trigger AS
$function$
  DECLARE
    instance_id uuid;
    occurred_at timestamptz;
    invalidations json;
    curr_rec record;
  BEGIN
    instance_id = NEW.instance_id;
    occurred_at = NEW.occurred_at;
    invalidations = NEW.invalidations;
    PERFORM pg_notify('hasura_schema_update', json_build_object(
      'instance_id', instance_id,
      'occurred_at', occurred_at,
      'invalidations', invalidations
      )::text);
    RETURN curr_rec;
  END;
$function$
LANGUAGE plpgsql;

CREATE TRIGGER hdb_schema_update_event_notifier AFTER INSERT OR UPDATE ON
  hdb_catalog.hdb_schema_update_event FOR EACH ROW EXECUTE PROCEDURE
  hdb_catalog.hdb_schema_update_event_notifier();

CREATE VIEW hdb_catalog.hdb_table_info_agg AS
  SELECT
    schema.nspname AS table_schema,
    "table".relname AS table_name,

    -- This field corresponds to the `CatalogTableInfo` Haskell type
    jsonb_build_object(
      'oid', "table".oid :: integer,
      'columns', coalesce(columns.info, '[]'),
      'primary_key', primary_key.info,
      -- Note: unique_constraints does NOT include primary key constraints!
      'unique_constraints', coalesce(unique_constraints.info, '[]'),
      'foreign_keys', coalesce(foreign_key_constraints.info, '[]'),
      'view_info', CASE "table".relkind WHEN 'v' THEN jsonb_build_object(
        'is_updatable', ((pg_catalog.pg_relation_is_updatable("table".oid, true) & 4) = 4),
        'is_insertable', ((pg_catalog.pg_relation_is_updatable("table".oid, true) & 8) = 8),
        'is_deletable', ((pg_catalog.pg_relation_is_updatable("table".oid, true) & 16) = 16)
      ) END,
      'description', description.description
    ) AS info

  -- table & schema
  FROM pg_catalog.pg_class "table"
  JOIN pg_catalog.pg_namespace schema
    ON schema.oid = "table".relnamespace

  -- description
  LEFT JOIN pg_catalog.pg_description description
    ON  description.classoid = 'pg_catalog.pg_class'::regclass
    AND description.objoid = "table".oid
    AND description.objsubid = 0

  -- columns
  LEFT JOIN LATERAL
    ( SELECT jsonb_agg(jsonb_build_object(
        'name', "column".attname,
        'position', "column".attnum,
        'type', coalesce(base_type.typname, "type".typname),
        'is_nullable', NOT "column".attnotnull,
        'description', pg_catalog.col_description("table".oid, "column".attnum)
      )) AS info
      FROM pg_catalog.pg_attribute "column"
      LEFT JOIN pg_catalog.pg_type "type"
        ON "type".oid = "column".atttypid
      LEFT JOIN pg_catalog.pg_type base_type
        ON "type".typtype = 'd' AND base_type.oid = "type".typbasetype
      WHERE "column".attrelid = "table".oid
        -- columns where attnum <= 0 are special, system-defined columns
        AND "column".attnum > 0
        -- dropped columns still exist in the system catalog as “zombie” columns, so ignore those
        AND NOT "column".attisdropped
    ) columns ON true

  -- primary key
  LEFT JOIN LATERAL
    ( SELECT jsonb_build_object(
        'constraint', jsonb_build_object('name', class.relname, 'oid', class.oid :: integer),
        'columns', coalesce(columns.info, '[]')
      ) AS info
      FROM pg_catalog.pg_index index
      JOIN pg_catalog.pg_class class
        ON class.oid = index.indexrelid
      LEFT JOIN LATERAL
        ( SELECT jsonb_agg("column".attname) AS info
          FROM pg_catalog.pg_attribute "column"
          WHERE "column".attrelid = "table".oid
            AND "column".attnum = ANY (index.indkey)
        ) AS columns ON true
      WHERE index.indrelid = "table".oid
        AND index.indisprimary
    ) primary_key ON true

  -- unique constraints
  LEFT JOIN LATERAL
    ( SELECT jsonb_agg(jsonb_build_object('name', class.relname, 'oid', class.oid :: integer)) AS info
      FROM pg_catalog.pg_index index
      JOIN pg_catalog.pg_class class
        ON class.oid = index.indexrelid
      WHERE index.indrelid = "table".oid
        AND index.indisunique
        AND NOT index.indisprimary
    ) unique_constraints ON true

  -- foreign keys
  LEFT JOIN LATERAL
    ( SELECT jsonb_agg(jsonb_build_object(
        'constraint', jsonb_build_object(
          'name', foreign_key.constraint_name,
          'oid', foreign_key.constraint_oid :: integer
        ),
        'columns', foreign_key.columns,
        'foreign_table', jsonb_build_object(
          'schema', foreign_key.ref_table_table_schema,
          'name', foreign_key.ref_table
        ),
        'foreign_columns', foreign_key.ref_columns
      )) AS info
      FROM hdb_catalog.hdb_foreign_key_constraint foreign_key
      WHERE foreign_key.table_schema = schema.nspname
        AND foreign_key.table_name = "table".relname
    ) foreign_key_constraints ON true

  -- all these identify table-like things
  WHERE "table".relkind IN ('r', 't', 'v', 'm', 'f', 'p');

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

CREATE OR REPLACE FUNCTION hdb_catalog.check_violation(msg text) RETURNS bool AS
$$
  BEGIN
    RAISE check_violation USING message=msg;
  END;
$$ LANGUAGE plpgsql;

CREATE TABLE hdb_catalog.hdb_action
(
  action_name TEXT PRIMARY KEY,
  action_defn JSONB NOT NULL,
  comment TEXT NULL,
  is_system_defined boolean default false
);

CREATE TABLE hdb_catalog.hdb_action_permission
(
  action_name TEXT NOT NULL,
  role_name TEXT NOT NULL,
  definition JSONB NOT NULL DEFAULT '{}'::jsonb,
  comment    TEXT NULL,

  PRIMARY KEY (action_name, role_name),
  FOREIGN KEY (action_name) REFERENCES hdb_catalog.hdb_action(action_name) ON UPDATE CASCADE
);

CREATE TABLE hdb_catalog.hdb_action_log
(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  -- we deliberately do not reference the action name
  -- because sometimes we may want to retain history
  -- after dropping the action
  action_name TEXT,
  input_payload JSONB NOT NULL,
  request_headers JSONB NOT NULL,
  session_variables JSONB NOT NULL,
  response_payload JSONB NULL,
  errors JSONB NULL,
  created_at timestamptz NOT NULL default now(),
  response_received_at timestamptz NULL,
  status text NOT NULL,
  CHECK (status IN ('created', 'processing', 'completed', 'error'))
);

CREATE TABLE hdb_catalog.hdb_custom_types
(
  custom_types jsonb NOT NULL
);

CREATE VIEW hdb_catalog.hdb_role AS
(
  SELECT DISTINCT role_name FROM (
    SELECT role_name FROM hdb_catalog.hdb_permission
    UNION ALL
    SELECT role_name FROM hdb_catalog.hdb_action_permission
  ) q
);

CREATE TABLE hdb_catalog.hdb_scheduled_trigger
(
  name TEXT PRIMARY KEY,
  webhook_conf JSON NOT NULL,
  schedule_conf JSON NOT NULL,
  payload JSON,
  retry_conf JSON,
  header_conf JSON,
  include_in_metadata BOOLEAN NOT NULL DEFAULT FALSE,
  comment TEXT
);

CREATE TABLE hdb_catalog.hdb_scheduled_events
(
  id TEXT DEFAULT gen_random_uuid() UNIQUE,
  name TEXT,
  scheduled_time TIMESTAMPTZ NOT NULL,
  additional_payload JSON,
  status TEXT NOT NULL DEFAULT 'scheduled',
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  next_retry_at TIMESTAMPTZ,

  PRIMARY KEY (name, scheduled_time),
  FOREIGN KEY (name) REFERENCES hdb_catalog.hdb_scheduled_trigger(name)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT valid_status CHECK (status IN ('scheduled','locked','delivered','error','dead'))
);

CREATE INDEX hdb_scheduled_event_status ON hdb_catalog.hdb_scheduled_events (status);

CREATE TABLE hdb_catalog.hdb_scheduled_event_invocation_logs
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMP DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.hdb_scheduled_events (id) ON DELETE CASCADE
);

CREATE VIEW hdb_catalog.hdb_scheduled_events_stats AS
  SELECT st.name,
         COALESCE(ste.upcoming_events_count,0) as upcoming_events_count,
         COALESCE(ste.max_scheduled_time, now()) as max_scheduled_time
  FROM hdb_catalog.hdb_scheduled_trigger st
  LEFT JOIN
  ( SELECT name, count(*) as upcoming_events_count, max(scheduled_time) as max_scheduled_time
    FROM hdb_catalog.hdb_scheduled_events
    WHERE tries = 0
    GROUP BY name
  ) ste
  ON st.name = ste.name;

CREATE TABLE hdb_catalog.hdb_one_off_scheduled_events
(
  id TEXT DEFAULT gen_random_uuid() UNIQUE,
  webhook_conf JSON NOT NULL,
  scheduled_time TIMESTAMPTZ NOT NULL,
  retry_conf JSON,
  payload JSON,
  header_conf JSON,
  status TEXT NOT NULL DEFAULT 'scheduled',
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  comment TEXT,
  CONSTRAINT valid_status CHECK (status IN ('scheduled','locked','delivered','error','dead'))
);

CREATE INDEX hdb_hdb_one_off_scheduled_event_status ON hdb_catalog.hdb_one_off_scheduled_events (status);
