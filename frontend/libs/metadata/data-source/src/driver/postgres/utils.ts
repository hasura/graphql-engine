import { RunSQLResponse } from '@hasura/shared/types';
import { IntrospectedTable, TableColumn } from '../types';
import { parseCreateSQL } from '../common/sqlUtils';
import { consoleDataTypeToSQLTypeMap } from './types';
import {
  formatPostgresArrayLiteral,
  isArrayString,
  parsePostgresArrayLiteral,
} from '@hasura/shared/utils';
import { getValidateInputRowValuesFunction } from '../common/validation';

const UPPERCASE_REGEX = /[A-Z]/;

function containsUppercase(str: string) {
  return UPPERCASE_REGEX.test(str);
}

export function adaptStringForPostgres(str: string) {
  return containsUppercase(str) ? `"${str}"` : str;
}

export function adaptSQLDataType(
  sqlDataType: string,
): TableColumn['consoleDataType'] {
  // `pg_catalog.format_type` names array types after their element, e.g. `text[]`
  if (sqlDataType.endsWith('[]')) return 'array';

  const [dataType] = Object.entries(consoleDataTypeToSQLTypeMap).find(
    ([, value]) => value.includes(sqlDataType),
  ) ?? ['string', []];

  return dataType as TableColumn['consoleDataType'];
}

export const adaptIntrospectedTables = ([
  fullTableListResult,
  partitionsTablesResult,
]: RunSQLResponse[]): IntrospectedTable[] => {
  const partitionNames =
    partitionsTablesResult.result?.map((row) => row[0]) ?? [];
  /* 
    The `slice(1)` on the result is done because the first item of the result is always the columns names from the SQL output.
    It is not required for the final result and should be avoided 
  */
  const adaptedResponse = fullTableListResult?.result
    ?.slice(1)
    .map((row: string[]) => ({
      name: `${row[1]}.${row[0]}`,
      table: {
        name: row[0],
        schema: row[1],
      },
      type: partitionNames.includes(row[0]) ? 'PARTITION' : row[2],
    }));

  return adaptedResponse ?? [];
};

export const isPostgresTable = (tableType: string): boolean => {
  return (
    tableType === 'TABLE' ||
    tableType === 'BASE TABLE' ||
    tableType === 'PARTITIONED TABLE' ||
    tableType === 'FOREIGN TABLE'
  );
};

const createSQLRegex =
  /create\s*(?:|or\s*replace)\s*(?<type>view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((?<schema>\"?\w+\"?)\.(?<nameWithSchema>\"?\w+\"?)|(?<name>\"?\w+\"?))\s*(?<partition>partition\s*of)?/gim; // eslint-disable-line

export const parsePostgresCreateSchemaSQL = (sql: string) =>
  parseCreateSQL(sql, 'postgres', createSQLRegex);

export const arrayToPostgresArray = (rawValue: unknown[]) =>
  formatPostgresArrayLiteral(rawValue);

/** The element type of an array column, e.g. `integer` for `integer[]`. */
export const adaptSQLArrayItemType = (
  sqlDataType: string,
): TableColumn['arrayItemType'] => {
  if (!sqlDataType.endsWith('[]')) return undefined;

  const itemType = adaptSQLDataType(sqlDataType.slice(0, -2));
  // a multi-dimensional array is edited as text items
  return itemType === 'array' ? 'string' : itemType;
};

export const validatePostgresInputRowValues = getValidateInputRowValuesFunction(
  {
    // the GraphQL API takes Postgres arrays as JSON arrays, so a JSON or
    // Postgres literal text (`{a,b}`) is read into one
    array: (column: string, rawValue: unknown) => {
      if (Array.isArray(rawValue)) {
        return rawValue;
      }

      if (!rawValue) {
        return null;
      }

      if (typeof rawValue === 'string') {
        if (isArrayString(rawValue)) return JSON.parse(rawValue);

        const items = parsePostgresArrayLiteral(rawValue);
        if (items) return items;
      }

      throw new Error(
        `${column} ' :: could not read ${rawValue} as a valid array'`,
      );
    },
  },
);
