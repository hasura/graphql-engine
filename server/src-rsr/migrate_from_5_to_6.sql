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
    min(q.confdeltype) :: text as on_delete
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
