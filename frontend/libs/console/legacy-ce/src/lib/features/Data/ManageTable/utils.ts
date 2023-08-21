import isPlainObject from 'lodash/isPlainObject';
import { Table } from '../../hasura-metadata-types';

type PostgresOrMssqlTable = {
  name: string;
  schema: string;
};
type BigQueryTable = {
  name: string;
  dataset: string;
};

function isRecordObject(x: unknown): x is Record<string, unknown> {
  return isPlainObject(x);
}

export function hasNameAndSchema(
  table: unknown
): table is PostgresOrMssqlTable {
  return isRecordObject(table) && 'schema' in table && 'name' in table;
}
export function isBigQueryTable(table: unknown): table is BigQueryTable {
  return isRecordObject(table) && 'dataset' in table && 'name' in table;
}

export const getQualifiedTable = (table: Table): string[] => {
  if (Array.isArray(table)) return table;

  // This is a safe assumption to make because the only native database that supports functions is postgres( and variants)
  if (typeof table === 'string') return ['public', table];

  //postgres and variants OR mssql
  if (hasNameAndSchema(table)) {
    return [table.schema, table.name];
  }

  if (isBigQueryTable(table)) {
    return [table.dataset, table.name];
  }

  return [];
};
