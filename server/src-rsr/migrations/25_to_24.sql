CREATE OR REPLACE VIEW hdb_catalog.hdb_column AS
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
LEFT JOIN primary_key_references AS pkey_refs
           ON columns.table_schema = pkey_refs.src_table_schema
          AND columns.table_name   = pkey_refs.src_table_name
          AND columns.column_name  = pkey_refs.src_column_name
LEFT JOIN pg_class ON pg_class.relname = columns.table_name
LEFT JOIN pg_namespace ON pg_namespace.oid = pg_class.relnamespace
          AND pg_namespace.nspname = columns.table_schema;
