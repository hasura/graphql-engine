import { runDatabaseMigration } from '@hasura/metadata/api';
import { InsertRowProps, TableColumn } from '../../types';
import { PostgresTable } from '../types';
import { validatePostgresInputRowValues } from '../utils';

type Row = Record<string, unknown>;

const quoteIdentifier = (name: string) => `"${name.replace(/"/g, '""')}"`;

const quoteLiteral = (value: string) => `'${value.replace(/'/g, "''")}'`;

const toArrayElement = (value: unknown): string => {
  if (value === null || value === undefined) return 'NULL';
  if (Array.isArray(value)) return `{${value.map(toArrayElement).join(',')}}`;
  if (typeof value === 'number' || typeof value === 'boolean') {
    return String(value);
  }
  const str = typeof value === 'object' ? JSON.stringify(value) : String(value);
  return `"${str.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`;
};

/** Converts a value returned by the `/v2/query` API into a SQL literal. */
export const toSQLLiteral = (value: unknown): string => {
  if (value === null || value === undefined) return 'NULL';
  if (typeof value === 'number' || typeof value === 'boolean') {
    return String(value);
  }
  if (Array.isArray(value)) {
    return quoteLiteral(`{${value.map(toArrayElement).join(',')}}`);
  }
  if (typeof value === 'object') return quoteLiteral(JSON.stringify(value));
  return quoteLiteral(String(value));
};

const qualifiedTableName = (table: PostgresTable) =>
  `${quoteIdentifier(table.schema)}.${quoteIdentifier(table.name)}`;

export const getInsertRowSQL = (table: PostgresTable, row: Row) => {
  const columnNames = Object.keys(row).map(quoteIdentifier).join(', ');
  const values = Object.values(row).map(toSQLLiteral).join(', ');
  return `INSERT INTO ${qualifiedTableName(table)} (${columnNames}) VALUES (${values});`;
};

/**
 * Deletes the inserted row, identified by its primary key or, for tables
 * without one, by all its column values.
 */
export const getDeleteRowSQL = (
  table: PostgresTable,
  row: Row,
  columns: TableColumn[],
) => {
  const primaryKeys = columns
    .filter((column) => column.isPrimaryKey)
    .map((column) => column.name);
  const keys = primaryKeys.length ? primaryKeys : Object.keys(row);

  const condition = keys
    .map((key) =>
      row[key] === null || row[key] === undefined
        ? `${quoteIdentifier(key)} IS NULL`
        : `${quoteIdentifier(key)} = ${toSQLLiteral(row[key])}`,
    )
    .join(' AND ');

  return `DELETE FROM ${qualifiedTableName(table)} WHERE ${condition};`;
};

export async function insertRows({
  args,
  fetchJson,
  endpoints,
}: InsertRowProps): Promise<number> {
  const objects = args.objects.map((obj) =>
    validatePostgresInputRowValues({
      columns: args.columns,
      values: obj,
      graphqlMode: false,
    }),
  );

  const result = (await fetchJson(endpoints.queryV2, {
    method: 'POST',
    body: JSON.stringify({
      type: 'insert',
      args: {
        objects,
        // the inserted rows (with generated values) are needed for the migration
        returning: args.isMigration
          ? args.columns.map((column) => column.name)
          : undefined,
        source: args.source.name,
        table: args.table,
      },
    }),
  })) as { affected_rows: number; returning?: Row[] };

  if (!result || typeof result !== 'object' || !('affected_rows' in result)) {
    return 0;
  }

  if (args.isMigration && result.returning?.length) {
    const table = args.table as PostgresTable;
    try {
      // the rows are already inserted: only write the migration files
      await runDatabaseMigration({
        endpoints,
        fetchJson,
        isMigration: true,
        args: {
          name: `insert_into_${table.schema}_${table.name}`,
          source: { name: args.source.name, kind: args.source.kind },
          up: result.returning.map((row) => ({
            sql: getInsertRowSQL(table, row),
          })),
          down: result.returning.map((row) => ({
            sql: getDeleteRowSQL(table, row, args.columns),
          })),
          skip_execution: true,
        },
      });
    } catch (error) {
      throw Object.assign(
        new Error(
          `The row was inserted, but creating the migration failed: ${
            error instanceof Error ? error.message : String(error)
          }`,
        ),
        { cause: error },
      );
    }
  }

  return result.affected_rows;
}
