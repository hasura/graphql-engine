ALTER TABLE hdb_catalog.hdb_table
  ADD COLUMN is_enum boolean NOT NULL DEFAULT false;

DROP TRIGGER hdb_table_oid_check ON hdb_catalog.hdb_table;
DROP FUNCTION hdb_catalog.hdb_table_oid_check();

CREATE OR REPLACE VIEW hdb_catalog.hdb_foreign_key_constraint AS
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
     FROM information_schema.columns
LEFT JOIN primary_key_references AS pkey_refs
           ON columns.table_schema = pkey_refs.src_table_schema
          AND columns.table_name   = pkey_refs.src_table_name
          AND columns.column_name  = pkey_refs.src_column_name;

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
          'name', name,
          'type', type,
          'is_nullable', is_nullable :: boolean,
          'references', primary_key_references
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
);
