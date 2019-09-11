-- Modify hdb_primary_key and hdb_unique_constraint views to have constraint_oid
CREATE OR REPLACE VIEW hdb_catalog.hdb_primary_key AS
SELECT
  tc.table_schema,
  tc.table_name,
  tc.constraint_name,
  json_agg(constraint_column_usage.column_name) AS columns,
  constraint_column_usage.constraint_oid
FROM
  (
    information_schema.table_constraints tc
    JOIN (
      SELECT
        x.tblschema AS table_schema,
        x.tblname AS table_name,
        x.colname AS column_name,
        x.cstrname AS constraint_name,
        x.cstroid AS constraint_oid
      FROM
        (
          SELECT
            DISTINCT nr.nspname,
            r.relname,
            a.attname,
            c.conname,
            c.oid
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
            c.conname,
            c.oid
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
          cstrname,
          cstroid
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
  tc.constraint_name,
  constraint_column_usage.constraint_oid;

CREATE OR REPLACE VIEW hdb_catalog.hdb_unique_constraint AS
  SELECT
    tc.table_name,
    tc.constraint_schema AS table_schema,
    tc.constraint_name as constraint_name,
    json_agg(kcu.column_name) AS columns,
    pc.oid as constraint_oid
    FROM
        information_schema.table_constraints tc
        JOIN information_schema.key_column_usage AS kcu
            USING (constraint_schema, constraint_name)
        JOIN pg_constraint pc ON (pc.conname = tc.constraint_name)
   WHERE
    constraint_type = 'UNIQUE'
   GROUP BY
    tc.table_name, tc.constraint_schema, tc.constraint_name, pc.oid;

-- Drop primary_key_columns column from hdb_table_info_agg view
DROP VIEW hdb_catalog.hdb_table_info_agg;

CREATE VIEW hdb_catalog.hdb_table_info_agg AS (
select
  tables.table_name as table_name,
  tables.table_schema as table_schema,
  coalesce(columns.columns, '[]') as columns,
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
    select
      tc.table_schema,
      tc.table_name,
      json_agg(json_build_object('type', tc.type, 'name', tc.constraint_name, 'columns', tc.columns)) as constraints
    from
        (
          select
            table_schema,
            table_name,
            constraint_name,
            columns,
            'PRIMARY KEY' AS type
          from hdb_catalog.hdb_primary_key
          union all
          select
            table_schema,
            table_name,
            constraint_name,
            columns,
            'UNIQUE' AS type
          from hdb_catalog.hdb_unique_constraint
        ) tc
     group by
      tc.table_schema,
      tc.table_name
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
