import {
  getSchemasWhereClause,
  getTablesWhereClause,
} from '../common/sqlQueries';
import { Table } from '@hasura/shared/types';
import { TableORSchemaArg } from '../types';
import { PostgresTable } from './types';

export const getCreateSchemaSql = (schemaName: string) =>
  `create schema "${schemaName}";`;

export const getDropSchemaSql = (schemaName: string, cascade?: boolean) =>
  `DROP SCHEMA "${schemaName}"${cascade ? ' CASCADE' : ''};`;

export const getDropConstraintSql = ({
  table,
  constraintName,
}: {
  table: Table;
  constraintName: string;
}) => {
  const { schema, name } = table as PostgresTable;
  return `
  alter table "${schema}"."${name}" drop constraint "${constraintName}";`;
};

/** `ALTER TABLE ... ADD CONSTRAINT ... CHECK (...)` for the Postgres family. */
export const getCreateCheckConstraintSql = ({
  table,
  constraintName,
  check,
}: {
  table: Table;
  constraintName: string;
  check: string;
}) => {
  const { schema, name } = table as PostgresTable;
  return `alter table "${schema}"."${name}" add constraint "${constraintName}" check (${check});`;
};

const quoteCols = (columns: string[]) =>
  columns.map((c) => `"${c}"`).join(', ');

/** `ALTER TABLE ... ADD CONSTRAINT ... PRIMARY KEY (...)`. */
export const getCreatePrimaryKeySql = ({
  table,
  constraintName,
  columns,
}: {
  table: Table;
  constraintName: string;
  columns: string[];
}) => {
  const { schema, name } = table as PostgresTable;
  return `alter table "${schema}"."${name}" add constraint "${constraintName}" primary key (${quoteCols(
    columns,
  )});`;
};

/** Transactional drop + re-add of the primary key (mirrors legacy getAlterPkSql). */
export const getAlterPrimaryKeySql = ({
  table,
  constraintName,
  columns,
}: {
  table: Table;
  constraintName: string;
  columns: string[];
}) => {
  const { schema, name } = table as PostgresTable;
  return `BEGIN TRANSACTION;
ALTER TABLE "${schema}"."${name}" DROP CONSTRAINT "${constraintName}";
ALTER TABLE "${schema}"."${name}" ADD CONSTRAINT "${constraintName}" PRIMARY KEY (${quoteCols(
    columns,
  )});
COMMIT TRANSACTION;`;
};

/** `ALTER TABLE ... ADD CONSTRAINT ... UNIQUE (...)`. */
export const getCreateUniqueKeySql = ({
  table,
  constraintName,
  columns,
}: {
  table: Table;
  constraintName: string;
  columns: string[];
}) => {
  const { schema, name } = table as PostgresTable;
  return `alter table "${schema}"."${name}" add constraint "${constraintName}" unique (${quoteCols(
    columns,
  )});`;
};

export type PostgresIndexType =
  'btree' | 'hash' | 'gin' | 'gist' | 'spgist' | 'brin';

/** `CREATE [UNIQUE] INDEX "name" ON "s"."t" USING <type> (...)`. */
export const getCreateIndexSql = ({
  table,
  indexName,
  indexType,
  columns,
  unique = false,
}: {
  table: Table;
  indexName: string;
  indexType: PostgresIndexType;
  columns: string[];
  unique?: boolean;
}) => {
  const { schema, name } = table as PostgresTable;
  return `create ${
    unique ? 'unique ' : ''
  }index "${indexName}" on "${schema}"."${name}" using ${indexType} (${quoteCols(
    columns,
  )});`;
};

export const getDropIndexSql = ({
  table,
  indexName,
}: {
  table: Table;
  indexName: string;
}) => {
  const { schema } = table as PostgresTable;
  return `drop index if exists "${schema}"."${indexName}";`;
};

export const getDropTriggerSql = ({
  table,
  triggerName,
}: {
  table: Table;
  triggerName: string;
}) => {
  const { schema, name } = table as PostgresTable;
  return `drop trigger "${triggerName}" on "${schema}"."${name}";`;
};

/**
 * Catalog query returning the triggers for a single table (grouped by name),
 * one row per trigger.
 * Columns: trigger_name, action_timing, events, action_statement.
 */
/**
 * User functions returning `trigger`, i.e. usable in `CREATE TRIGGER`.
 * Columns: function_schema, function_name.
 */
export const getTriggerFunctionsSql = () => `
    SELECT n.nspname AS function_schema, p.proname AS function_name
    FROM pg_catalog.pg_proc p
      JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
    WHERE p.prorettype = 'pg_catalog.trigger'::pg_catalog.regtype
      AND n.nspname NOT IN ('pg_catalog', 'information_schema', 'hdb_catalog')
      AND n.nspname NOT LIKE 'pg\\_%'
    ORDER BY n.nspname, p.proname;
  `;

/**
 * Triggers of a table, one row per trigger. Columns: trigger_name,
 * action_timing, events, action_statement and, unless `withDefinition` is
 * false (CockroachDB lacks `pg_get_triggerdef`), trigger_definition — the full
 * `CREATE TRIGGER` statement.
 */
export const getTableTriggersSql = (
  table: Table,
  { withDefinition = true }: { withDefinition?: boolean } = {},
) => {
  const { schema, name } = table as PostgresTable;
  const definitionColumn = withDefinition
    ? `,
      (
        SELECT pg_catalog.pg_get_triggerdef(t.oid, true)
        FROM pg_catalog.pg_trigger t
          JOIN pg_catalog.pg_class c ON c.oid = t.tgrelid
          JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
        WHERE t.tgname = tr.trigger_name
          AND n.nspname = '${schema}'
          AND c.relname = '${name}'
      ) AS trigger_definition`
    : '';
  return `
    SELECT
      tr.trigger_name,
      tr.action_timing,
      string_agg(DISTINCT tr.event_manipulation, ', ') AS events,
      tr.action_statement${definitionColumn}
    FROM information_schema.triggers tr
    WHERE tr.event_object_schema = '${schema}'
      AND tr.event_object_table = '${name}'
    GROUP BY tr.trigger_name, tr.action_timing, tr.action_statement
    ORDER BY tr.trigger_name;
  `;
};

/**
 * Catalog query returning the indexes for a single table, one row per index.
 * Columns: index_name, index_type, index_columns (text[] in index key order),
 * index_definition_sql.
 */
export const getTableIndexesSql = (table: Table) => {
  const { schema, name } = table as PostgresTable;
  return `
    SELECT
      i.relname AS index_name,
      am.amname AS index_type,
      array_agg(a.attname::text ORDER BY array_position(ix.indkey::int2[], a.attnum)) AS index_columns,
      pi.indexdef AS index_definition_sql
    FROM pg_class t, pg_class i, pg_index ix, pg_attribute a,
      pg_namespace n, pg_am am, pg_indexes pi
    WHERE t.oid = ix.indrelid
      and i.oid = ix.indexrelid
      and a.attrelid = t.oid
      and a.attnum = ANY(ix.indkey)
      and t.relkind = 'r'
      and pi.indexname = i.relname
      and pi.tablename = t.relname
      and pi.schemaname = n.nspname
      and t.relname = '${name}'
      and n.nspname = '${schema}'
      and n.oid = t.relnamespace
      and n.oid = i.relnamespace
      and am.oid = i.relam
    GROUP BY i.relname, am.amname, pi.indexdef
    ORDER BY i.relname;
  `;
};

/**
 * Primary-key or unique constraints, one row per constraint.
 * Columns: table_name, table_schema, constraint_name, columns (text[] in key
 * order).
 */
export const getKeysSql = (
  type: 'PRIMARY KEY' | 'UNIQUE',
  options: TableORSchemaArg,
) => `
  -- test_id = ${'schemas' in options ? 'multi' : 'single'}_${
    type === 'UNIQUE' ? 'unique' : 'primary'
  }_key
  SELECT
      tc.table_name,
      tc.constraint_schema AS table_schema,
      tc.constraint_name,
      array_agg(kcu.column_name::text ORDER BY kcu.ordinal_position) AS columns
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
      tc.constraint_name
  ORDER BY
      tc.table_name,
      tc.constraint_name;
  `;

/**
 * Check constraints, one row per constraint.
 * Columns: table_schema, table_name, constraint_name, check.
 */
export const checkConstraintsSql = (options: TableORSchemaArg): string => {
  return `
-- test_id = ${'schemas' in options ? 'multi' : 'single'}_check_constraint
SELECT n.nspname::text AS table_schema,
    ct.relname::text AS table_name,
    r.conname::text AS constraint_name,
    pg_get_constraintdef(r.oid, true) AS "check"
   FROM pg_constraint r
     JOIN pg_class ct ON r.conrelid = ct.oid
     JOIN pg_namespace n ON ct.relnamespace = n.oid
  ${
    'schemas' in options
      ? getSchemasWhereClause(options.schemas)('n.nspname')
      : getTablesWhereClause(options.tables)({
          name: 'ct.relname',
          schema: 'n.nspname',
        })
  }
   AND r.contype = 'c'::"char"
 ORDER BY n.nspname, ct.relname, r.conname;
`;
};

export const statementTimeoutSQL = (statementTimeoutInSecs: number): string => {
  return `SET LOCAL statement_timeout = ${statementTimeoutInSecs * 1000};`;
};
