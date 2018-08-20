CREATE VIEW hdb_views.hdb_permission_agg AS
SELECT
    table_schema,
    table_name,
    role_name,
    json_object_agg(perm_type, perm_def) as permissions
FROM
    hdb_catalog.hdb_permission
GROUP BY
    table_schema, table_name, role_name;

CREATE VIEW hdb_views.hdb_foreign_key_constraint AS
SELECT
    q.table_schema :: text,
    q.table_name :: text,
    q.constraint_name :: text,
    hdb_catalog.first(q.constraint_oid) :: integer as constraint_oid,
    hdb_catalog.first(q.ref_table_table_schema) :: text as ref_table_table_schema,
    hdb_catalog.first(q.ref_table) :: text as ref_table,
    json_object_agg(ac.attname, afc.attname) as column_mapping,
    hdb_catalog.first(q.confupdtype) :: text as on_update,
    hdb_catalog.first(q.confdeltype) :: text as on_delete
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


CREATE VIEW hdb_views.hdb_check_constraint AS
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

CREATE VIEW hdb_views.hdb_unique_constraint AS
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

CREATE VIEW hdb_views.hdb_primary_key AS
SELECT
    tc.table_schema,
    tc.table_name,
    tc.constraint_name,
    json_agg(ccu.column_name) as columns
FROM
    information_schema.table_constraints tc
    JOIN information_schema.constraint_column_usage ccu
    ON tc.constraint_name = ccu.constraint_name
WHERE
    constraint_type = 'PRIMARY KEY'
GROUP BY
    tc.table_schema, tc.table_name, tc.constraint_name;

CREATE VIEW hdb_views.hdb_table_constraint AS
SELECT
    tc.table_schema,
    tc.table_name,
    tc.constraint_name,
    tc.constraint_type,
    r.oid::integer as constraint_oid
FROM
    information_schema.table_constraints tc
    JOIN pg_catalog.pg_constraint r
    ON tc.constraint_name = r.conname;

CREATE VIEW hdb_views.hdb_table_meta AS (
    SELECT
        t.table_schema,
        t.table_name,
        t.table_oid,
        c.columns,
        coalesce(f.constraints, '[]') as constraints
    FROM
        (SELECT
             c.oid as table_oid,
             c.relname as table_name,
             n.nspname as table_schema
         FROM
             pg_catalog.pg_class c
         JOIN
             pg_catalog.pg_namespace as n
           ON
             c.relnamespace = n.oid
        ) t
        INNER JOIN
        (SELECT
             table_schema,
             table_name,
             json_agg((SELECT r FROM (SELECT column_name, udt_name AS data_type, ordinal_position, is_nullable::boolean) r)) as columns
         FROM
             information_schema.columns
         GROUP BY
             table_schema, table_name) c
        ON (t.table_schema = c.table_schema AND t.table_name = c.table_name)
        LEFT OUTER JOIN
        (SELECT
             table_schema,
             table_name,
             json_agg((SELECT r FROM (SELECT constraint_name, constraint_oid, constraint_type) r)) as constraints
         FROM
             hdb_views.hdb_table_constraint r
         GROUP BY
             table_schema, table_name) f
        ON (t.table_schema = f.table_schema AND t.table_name = f.table_name)
    WHERE
        t.table_schema NOT LIKE 'pg_%'
        AND t.table_schema <> 'information_schema'
)
