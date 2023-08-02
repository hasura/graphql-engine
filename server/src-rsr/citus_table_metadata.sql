SELECT
  "table".table_schema,
  "table".table_name,

  -- This field corresponds to the `DBTableMetadata` Haskell type
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
    'description', description.description,
    'extra_table_metadata',
      CASE extraMetadata.citus_table_type
        WHEN 'distributed'
          THEN jsonb_build_object(
                  'tag', extraMetadata.citus_table_type,
                  'distribution_column', extraMetadata.distribution_column,
                  'table_type', CASE WHEN "table".relkind IN ('v', 'm') THEN 'view' ELSE 'table' END
                )
        ELSE jsonb_build_object(
                  'tag', coalesce(extraMetadata.citus_table_type, 'local'),
                  'table_type', CASE WHEN "table".relkind IN ('v', 'm') THEN 'view' ELSE 'table' END
                )
      END
  )::json AS info

-- tracked tables
-- $1 parameter provides JSON array of tracked tables
FROM
  ( SELECT "tracked"."name" AS "table_name",
           "tracked"."schema" AS "table_schema"
      FROM jsonb_to_recordset($1::jsonb) AS "tracked"("schema" text, "name" text)
  ) "tracked_table"

-- table & schema
LEFT JOIN
  ( SELECT "table".oid,
           "table".relkind,
           "table".relname AS "table_name",
           "schema".nspname AS "table_schema"
      FROM pg_catalog.pg_class "table"
           JOIN pg_catalog.pg_namespace "schema"
               ON schema.oid = "table".relnamespace
  ) "table"
      ON  "table"."table_name" = "tracked_table"."table_name"
      AND "table"."table_schema" = "tracked_table"."table_schema"

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
      'type', json_build_object('name', (CASE WHEN "array_type".typname IS NULL
                                              THEN coalesce(base_type.typname, "type".typname)
                                              ELSE "array_type".typname || '[]' END),
                                'type', "type".typtype),
      'is_nullable', NOT "column".attnotnull,
      'description', pg_catalog.col_description("table".oid, "column".attnum),
      'mutability', jsonb_build_object(
        'is_insertable', NOT (identitypolyfill.attidentity = 'a' OR generatedpolyfill.attgenerated = 's'),
        'is_updatable', NOT (identitypolyfill.attidentity = 'a' OR generatedpolyfill.attgenerated = 's'))
    )) AS info
    FROM pg_catalog.pg_attribute "column"
    -- The columns 'pg_attribute.attidentity' and 'pg_attribute.attgenerated' are
    -- not available in older versions of Postgres, because those versions do not
    -- implement the concepts the catalog columns represent.
    -- To support older versions we apply an aliasing hack that ensures
    -- _something_ called e.g. attidentity is in scope.
    -- Originally sourced from: https://stackoverflow.com/questions/18951071/postgres-return-a-default-value-when-a-column-doesnt-exist.
    INNER JOIN
    (
      SELECT attrelid, attnum, attname, CASE WHEN attidentity_exists
                                        THEN attidentity::text
                                        ELSE ''::text
                                        END as attidentity
      FROM pg_catalog.pg_attribute
      CROSS JOIN (SELECT current_setting('server_version_num')::int >= 100000)
            AS attidentity(attidentity_exists)
    ) AS identitypolyfill
      ON  identitypolyfill.attrelid = "column".attrelid
      AND identitypolyfill.attnum = "column".attnum
      AND identitypolyfill.attname = "column".attname

    INNER JOIN
    (
      SELECT attrelid, attnum, attname, CASE WHEN attgenerated_exists
                                                 THEN attgenerated::text
                                                 ELSE ''::text
                                                 END as attgenerated
      FROM pg_catalog.pg_attribute
      CROSS JOIN (SELECT current_setting('server_version_num')::int >= 120000)
            AS attgenerated(attgenerated_exists)
    ) AS generatedpolyfill
      ON  generatedpolyfill.attrelid = "column".attrelid
      AND generatedpolyfill.attnum = "column".attnum
      AND generatedpolyfill.attname = "column".attname

    LEFT JOIN pg_catalog.pg_type "type"
      ON "type".oid = "column".atttypid
    LEFT JOIN pg_catalog.pg_type base_type
      ON "type".typtype = 'd' AND base_type.oid = "type".typbasetype
    LEFT JOIN pg_catalog.pg_type array_type
      ON array_type.typarray = "type".oid
    WHERE "column".attrelid = "table".oid
      -- columns where attnum <= 0 are special, system-defined columns
      AND "column".attnum > 0
      -- dropped columns still exist in the system catalog as “zombie” columns, so ignore those
      AND NOT "column".attisdropped
  ) columns ON true

-- primary key
LEFT JOIN LATERAL
  ( SELECT jsonb_build_object(
      'constraint', jsonb_build_object(
        'name', class.relname,
        'oid', class.oid :: integer),
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
  ( SELECT jsonb_agg(
      jsonb_build_object(
        'constraint', jsonb_build_object(
          'name', class.relname,
          'oid', class.oid :: integer
          ),
        'columns', coalesce(columns.info, '[]')
        )
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
    )) AS info -- This field corresponds to the `PGForeignKeyMetadata` Haskell type
    FROM (SELECT
             q.table_schema :: text,
             q.table_name :: text,
             q.constraint_name :: text,
             min(q.constraint_oid) :: integer as constraint_oid,
             min(q.ref_table_table_schema) :: text as ref_table_table_schema,
             min(q.ref_table) :: text as ref_table,
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
         GROUP BY q.table_schema, q.table_name, q.constraint_name
    ) foreign_key
    WHERE foreign_key.table_schema = "table".table_schema
      AND foreign_key.table_name = "table".table_name
  ) foreign_key_constraints ON true

LEFT JOIN LATERAL
  ( SELECT citus_table_type, distribution_column, table_name
    FROM citus_tables extraMetadata
    -- DO NOT SUBMIT: Should we compare columns of type 'name' by casting to 'text'?
    WHERE extraMetadata.table_name::text = "table".table_name::text ) extraMetadata ON true

-- all these identify table-like things
WHERE "table".relkind IN ('r', 't', 'v', 'm', 'f', 'p')
  -- and tables not from any system schemas
  AND "table".table_schema NOT LIKE 'pg\_%'
  AND "table".table_schema NOT IN ('information_schema', 'hdb_catalog');
