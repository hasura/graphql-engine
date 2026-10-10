import { PostgresFamilyDriver } from '@hasura/shared/types';
import { AddColumnArgs, AddColumnProps } from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { buildDefaultStatement } from './createTable';

/**
 * Builds the up/down SQL for adding a column to an existing table (mirrors the
 * legacy `addColSql`). Any dependent SQL (e.g. the `updated_at` trigger of a
 * frequently used column) runs after the column is added and is undone before
 * the column is dropped.
 */
export const getAddColumnSql = ({ table, column }: AddColumnArgs) => {
  const { schema, name: tableName } = table as PostgresTable;
  const qualifiedTable = `"${schema}"."${tableName}"`;
  const name = column.name.trim();

  let columnDef = `"${name}" ${column.type}`;
  if (!column.nullable) columnDef += ' NOT NULL';
  if (column.unique) columnDef += ' UNIQUE';
  columnDef += buildDefaultStatement(column);

  const up: string[] = [];
  const down: string[] = [];

  // gen_random_uuid() lives in pgcrypto before Postgres 13.
  if (String(column.default?.value ?? '').includes('gen_random_uuid()')) {
    up.push('CREATE EXTENSION IF NOT EXISTS pgcrypto;');
  }
  up.push(`ALTER TABLE ${qualifiedTable} ADD COLUMN ${columnDef};`);
  down.push(`ALTER TABLE ${qualifiedTable} DROP COLUMN "${name}";`);

  const dependent = column.dependentSQLGenerator?.(table, name);
  if (dependent) {
    up.push(dependent.upSql);
    if (dependent.downSql) down.unshift(dependent.downSql);
  }

  return { up: up.join('\n'), down: down.join('\n') };
};

export const addColumnCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: AddColumnProps) => {
    const { up, down } = getAddColumnSql(args);
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `alter_table_${schema}_${name}_add_column_${args.column.name.trim()}`,
        up: [{ sql: up }],
        down: [{ sql: down }],
      },
    }).then(() => true);
  };
};
