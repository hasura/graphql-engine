DROP VIEW hdb_catalog.hdb_table_info_agg;
DROP VIEW hdb_catalog.hdb_column;

-- we need to drop this view temporarily so we can change the type of hdb_permission columns
DROP VIEW hdb_catalog.hdb_permission_agg;

-- See Note [Reference system columns using type name] in initialise.sql
ALTER TABLE hdb_catalog.hdb_table
  ALTER COLUMN table_schema TYPE name,
  ALTER COLUMN table_name TYPE name;
ALTER TABLE hdb_catalog.hdb_relationship
  ALTER COLUMN table_schema TYPE name,
  ALTER COLUMN table_name TYPE name;
ALTER TABLE hdb_catalog.hdb_permission
  ALTER COLUMN table_schema TYPE name,
  ALTER COLUMN table_name TYPE name;

-- this just recreates the view we dropped above with no actual changes
CREATE VIEW hdb_catalog.hdb_permission_agg AS
SELECT
  table_schema,
  table_name,
  role_name,
  json_object_agg(perm_type, perm_def) as permissions
FROM hdb_catalog.hdb_permission
GROUP BY table_schema, table_name, role_name;

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