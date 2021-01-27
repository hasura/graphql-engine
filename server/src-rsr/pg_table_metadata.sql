SELECT
  schema.nspname AS table_schema,
  "table".relname AS table_name,

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
    'description', description.description
  )::json AS info

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
    WHERE foreign_key.table_schema = schema.nspname
      AND foreign_key.table_name = "table".relname
  ) foreign_key_constraints ON true

-- all these identify table-like things
WHERE "table".relkind IN ('r', 't', 'v', 'm', 'f', 'p')
  -- and tables not from any system schemas
  AND schema.nspname NOT LIKE 'pg_%'
  AND schema.nspname NOT IN ('information_schema', 'hdb_catalog');
