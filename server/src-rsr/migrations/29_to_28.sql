DROP VIEW hdb_catalog.hdb_table_info_agg;
DROP VIEW hdb_catalog.hdb_permission_agg;

ALTER TABLE hdb_catalog.hdb_table
  ALTER COLUMN table_schema TYPE text,
  ALTER COLUMN table_name TYPE text;
ALTER TABLE hdb_catalog.hdb_relationship
  ALTER COLUMN table_schema TYPE text,
  ALTER COLUMN table_name TYPE text;
ALTER TABLE hdb_catalog.hdb_permission
  ALTER COLUMN table_schema TYPE text,
  ALTER COLUMN table_name TYPE text;

CREATE VIEW hdb_catalog.hdb_permission_agg AS
SELECT
  table_schema,
  table_name,
  role_name,
  json_object_agg(perm_type, perm_def) as permissions
FROM hdb_catalog.hdb_permission
GROUP BY table_schema, table_name, role_name;

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
