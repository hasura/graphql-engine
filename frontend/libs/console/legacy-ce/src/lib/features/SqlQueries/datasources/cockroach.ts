import type { TableORSchemaArg } from '../../../dataSources/types';
import { QualifiedTable } from '../../../metadata/types';
import type { DatasourceSqlQueries } from '.';
import { getSchemasWhereClause, getTablesWhereClause } from './common';

const getKeysSql = (
  type: 'PRIMARY KEY' | 'UNIQUE',
  options: TableORSchemaArg
) => `
  -- test_id = ${'schemas' in options ? 'multi' : 'single'}_${
  type === 'UNIQUE' ? 'unique' : 'primary'
}_key
  SELECT
  COALESCE(
    json_agg(
      row_to_json(info)
    ),
    '[]' :: JSON
  )
  FROM (
      SELECT
          tc.table_name,
          tc.constraint_schema AS table_schema,
          tc.constraint_name,
          json_agg(kcu.column_name) AS columns
      FROM
          information_schema.table_constraints tc
          JOIN information_schema.key_column_usage kcu USING (constraint_schema, constraint_name)
    ${
      'schemas' in options
        ? getSchemasWhereClause(options.schemas)('tc.constraint_schema')
        : getTablesWhereClause(options.tables)({
            name: 'tc.table_name',
            schema: 'tc.constraint_schema',
          })
    }
          AND tc.constraint_type::text = '${type}'::text
      GROUP BY
          tc.table_name,
          tc.constraint_schema,
          tc.constraint_name) AS info;
  `;

const getTableColumnsSql = ({ name, schema }: QualifiedTable) => {
  if (!name || !schema) throw Error('empty parameters are not allowed!');

  return `SELECT table_catalog as database, table_schema, table_name, column_name, data_type FROM information_schema.columns WHERE table_schema = '${schema}' AND table_name  = '${name}';`;
};

export const cockroachDbSqlQueries: DatasourceSqlQueries = {
  getFetchTablesListQuery(options: TableORSchemaArg): string {
    return `
  -- test_id = ${'schemas' in options ? 'multi' : 'single'}_table
  SELECT
    COALESCE(Json_agg(Row_to_json(info)), '[]' :: json) AS tables
  FROM (
    WITH partitions AS (
      SELECT array(
        WITH partitioned_tables AS (SELECT array(SELECT oid FROM pg_class WHERE relkind = 'p') AS parent_tables)
        SELECT
        child.relname       AS partition
    FROM partitioned_tables, pg_inherits
        JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
        JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
    ${
      'schemas' in options
        ? getSchemasWhereClause(options.schemas)('nmsp_child.nspname')
        : getTablesWhereClause(options.tables)({
            name: 'child.relname',
            schema: 'nmsp_child.nspname',
          })
    }
    AND pg_inherits.inhparent = ANY (partitioned_tables.parent_tables)
      ) AS names
    )
    SELECT
      pgn.nspname AS table_schema,
      pgc.relname AS table_name,
      CASE
        WHEN pgc.relkind = 'r' THEN 'TABLE'
        WHEN pgc.relkind = 'f' THEN 'FOREIGN TABLE'
        WHEN pgc.relkind = 'v' THEN 'VIEW'
        WHEN pgc.relkind = 'm' THEN 'MATERIALIZED VIEW'
        WHEN pgc.relkind = 'p' THEN 'PARTITIONED TABLE'
      END AS table_type,
      obj_description(pgc.oid) AS comment,
      COALESCE(json_agg(DISTINCT row_to_json(isc) :: jsonb || jsonb_build_object('comment', col_description(pga.attrelid, pga.attnum))) filter (WHERE isc.column_name IS NOT NULL), '[]' :: json) AS columns,
      COALESCE(json_agg(DISTINCT row_to_json(ist) :: jsonb || jsonb_build_object('comment', obj_description(pgt.oid))) filter (WHERE ist.trigger_name IS NOT NULL), '[]' :: json) AS triggers,
      row_to_json(isv) AS view_info
      FROM partitions, pg_class as pgc  
      INNER JOIN pg_namespace as pgn
        ON pgc.relnamespace = pgn.oid
    /* columns */
    /* This is a simplified version of how information_schema.columns was
    ** implemented in postgres 9.5, but modified to support materialized
    ** views.
    */
    LEFT OUTER JOIN pg_attribute AS pga
      ON pga.attrelid = pgc.oid
    LEFT OUTER JOIN (
      SELECT
        nc.nspname         AS table_schema,
        c.relname          AS table_name,
        a.attname          AS column_name,
        a.attnum           AS ordinal_position,
        pg_get_expr(ad.adbin, ad.adrelid) AS column_default,
        CASE WHEN a.attnotnull OR (t.typtype = 'd' AND t.typnotnull) THEN 'NO' ELSE 'YES' END AS is_nullable,
        CASE WHEN t.typtype = 'd' THEN
          CASE WHEN bt.typelem <> 0 AND bt.typlen = -1 THEN 'ARRAY'
               WHEN nbt.nspname = 'pg_catalog' THEN format_type(t.typbasetype, null)
               ELSE 'USER-DEFINED' END
        ELSE
          CASE WHEN t.typelem <> 0 AND t.typlen = -1 THEN 'ARRAY'
               WHEN nt.nspname = 'pg_catalog' THEN format_type(a.atttypid, null)
               ELSE 'USER-DEFINED' END
        END AS data_type,
        coalesce(bt.typname, t.typname) AS data_type_name
      FROM (pg_attribute a LEFT JOIN pg_attrdef ad ON attrelid = adrelid AND attnum = adnum)
        JOIN (pg_class c JOIN pg_namespace nc ON (c.relnamespace = nc.oid)) ON a.attrelid = c.oid
        JOIN (pg_type t JOIN pg_namespace nt ON (t.typnamespace = nt.oid)) ON a.atttypid = t.oid
        LEFT JOIN (pg_type bt JOIN pg_namespace nbt ON (bt.typnamespace = nbt.oid))
          ON (t.typtype = 'd' AND t.typbasetype = bt.oid)
        LEFT JOIN (pg_collation co JOIN pg_namespace nco ON (co.collnamespace = nco.oid))
          ON a.attcollation = co.oid AND (nco.nspname, co.collname) <> ('pg_catalog', 'default')
      WHERE --(NOT pg_is_other_temp_schema(nc.oid))
        -- AND
          a.attnum > 0 AND NOT a.attisdropped AND c.relkind in ('r', 'v', 'm', 'f', 'p')
          AND (-- pg_has_role(c.relowner, 'USAGE')
              -- OR
                has_column_privilege(c.oid, a.attnum,
                                     'SELECT, INSERT, UPDATE, REFERENCES'))
    ) AS isc
      ON  isc.table_schema = pgn.nspname
      AND isc.table_name   = pgc.relname
      AND isc.column_name  = pga.attname
  
    /* triggers */
    LEFT OUTER JOIN pg_trigger AS pgt
      ON pgt.tgrelid = pgc.oid
    LEFT OUTER JOIN information_schema.triggers AS ist
      ON  ist.event_object_schema = pgn.nspname
      AND ist.event_object_table  = pgc.relname
      AND ist.trigger_name        = pgt.tgname
  
    /* This is a simplified version of how information_schema.views was
    ** implemented in postgres 9.5, but modified to support materialized
    ** views.
    */
    LEFT OUTER JOIN (
      SELECT
        nc.nspname         AS table_schema,
        c.relname          AS table_name,
        pg_get_viewdef(c.oid),
        -- CASE WHEN pg_has_role(c.relowner, 'USAGE') THEN pg_get_viewdef(c.oid) ELSE null END AS view_definition,
        CASE WHEN pg_relation_is_updatable(c.oid, false) & 20 = 20 THEN 'YES' ELSE 'NO' END AS is_updatable,
        CASE WHEN pg_relation_is_updatable(c.oid, false) &  8 =  8 THEN 'YES' ELSE 'NO' END AS is_insertable_into,
        CASE WHEN EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = c.oid AND tgtype & 81 = 81) THEN 'YES' ELSE 'NO' END AS is_trigger_updatable,
        CASE WHEN EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = c.oid AND tgtype & 73 = 73) THEN 'YES' ELSE 'NO' END AS is_trigger_deletable,
        CASE WHEN EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = c.oid AND tgtype & 69 = 69) THEN 'YES' ELSE 'NO' END AS is_trigger_insertable_into
      FROM pg_namespace nc, pg_class c
  
      WHERE c.relnamespace = nc.oid
        AND c.relkind in ('v', 'm')
        -- AND (NOT pg_is_other_temp_schema(nc.oid))
        AND (-- pg_has_role(c.relowner, 'USAGE')
             -- OR
             has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
             OR has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'))
    ) AS isv
      ON  isv.table_schema = pgn.nspname
      AND isv.table_name   = pgc.relname
  
    WHERE
      pgc.relkind IN ('r', 'v', 'f', 'm', 'p')
      ${
        'schemas' in options
          ? getSchemasWhereClause(options.schemas, 'AND')('pgn.nspname')
          : getTablesWhereClause(
              options.tables,
              'AND'
            )({
              name: 'pgc.relname',
              schema: 'pgn.nspname',
            })
      }
    GROUP BY pgc.oid, pgn.nspname, pgc.relname, table_type, isv.*
  ) AS info;
  `;
  },
  primaryKeysInfoSql(options: TableORSchemaArg): string {
    return getKeysSql('PRIMARY KEY', options);
  },
  uniqueKeysSql(options: TableORSchemaArg): string {
    return getKeysSql('UNIQUE', options);
  },
  checkConstraintsSql(options: TableORSchemaArg): string {
    return `
-- test_id = ${'schemas' in options ? 'multi' : 'single'}_check_constraint
SELECT
COALESCE(
  json_agg(
    row_to_json(info)
  ),
  '[]' :: JSON
)
FROM (
SELECT n.nspname::text AS table_schema,
    ct.relname::text AS table_name,
    r.conname::text AS constraint_name,
    ccu.column_name,
    pg_get_constraintdef(r.oid, true) AS "check"
   FROM pg_constraint r
     JOIN pg_class ct ON r.conrelid = ct.oid
     JOIN pg_namespace n ON ct.relnamespace = n.oid
     JOIN information_schema.constraint_column_usage ccu
          ON r.conname = ccu.constraint_name
          AND n.nspname = ccu.constraint_schema
  ${
    'schemas' in options
      ? getSchemasWhereClause(options.schemas)('n.nspname')
      : getTablesWhereClause(options.tables)({
          name: 'ct.relname',
          schema: 'n.nspname',
        })
  }
   AND r.contype = 'c'::"char"
   ) AS info;
`;
  },
  getFKRelations(options: TableORSchemaArg): string {
    return `
-- test_id = ${'schemas' in options ? 'multi' : 'single'}_foreign_key
SELECT
COALESCE(json_agg(row_to_json(info)), '[]'::JSON)
FROM (
SELECT
q.table_schema :: text AS table_schema,
q.table_name :: text AS table_name,
q.constraint_name :: text AS constraint_name,
min(q.ref_table_table_schema :: text) AS ref_table_table_schema,
min(q.ref_table :: text) AS ref_table,
json_object_agg(ac.attname, afc.attname) AS column_mapping,
min(q.confupdtype :: text) AS on_update,
min(q.confdeltype :: text) AS on_delete
FROM
(
  SELECT
    ctn.nspname AS table_schema,
    ct.relname AS table_name,
    r.conrelid AS table_id,
    r.conname AS constraint_name,
    cftn.nspname AS ref_table_table_schema,
    cft.relname AS ref_table,
    r.confrelid AS ref_table_id,
    r.confupdtype,
    r.confdeltype,
    unnest(r.conkey) AS column_id,
    unnest(r.confkey) AS ref_column_id
  FROM
    pg_constraint r
    JOIN pg_class ct ON r.conrelid = ct.oid
    JOIN pg_namespace ctn ON ct.relnamespace = ctn.oid
    JOIN pg_class cft ON r.confrelid = cft.oid
    JOIN pg_namespace cftn ON cft.relnamespace = cftn.oid
  WHERE
    r.contype = 'f' :: "char"
) q
JOIN pg_attribute ac ON q.column_id = ac.attnum
AND q.table_id = ac.attrelid
JOIN pg_attribute afc ON q.ref_column_id = afc.attnum
AND q.ref_table_id = afc.attrelid
${
  'schemas' in options
    ? getSchemasWhereClause(options.schemas)([
        'q.table_schema',
        'q.ref_table_table_schema',
      ])
    : getTablesWhereClause(options.tables)([
        { name: 'q.table_name', schema: 'q.table_schema' },
        { name: 'q.ref_table', schema: 'q.ref_table_table_schema' },
      ])
}
GROUP BY
  q.table_schema,
  q.table_name,
  q.constraint_name
  ) AS info;`;
  },
  getTableColumnsSql,
};
