import { getEntries } from '../../../components/Services/Data/Common/tsUtils';
import { RunSQLResponse } from '../api';
import { IntrospectedTable, TableColumn } from '../types';

function containsUppercase(str: string) {
  return /[A-Z]/.test(str);
}

export function adaptStringForPostgres(str: string) {
  return containsUppercase(str) ? `"${str}"` : str;
}

export const consoleDataTypeToSQLTypeMap: Record<
  TableColumn['consoleDataType'],
  string[]
> = {
  boolean: ['boolean', 'bool'],
  string: [
    'box',
    'character',
    'character varying',
    'circle',
    'line',
    'lseg',
    'macaddr',
    'macaddr8',
    'path',
    'pg_lsn',
    'pg_snapshot',
    'point',
    'polygon',
    'tsquery',
    'tsvector',
    'txid_snapshot',
    'uuid',
    'char',
    'varchar',
    'date',
    'time',
    'timetz',
    'timestamptz',
    'timestamp',
  ],
  text: ['text'],
  number: [
    'bigint',
    'bigserial',
    'bit',
    'bit varying',
    'bytea',
    'cidr',
    'double precision',
    'inet',
    'integer',
    'money',
    'numeric',
    'real',
    'smallint',
    'smallserial',
    'serial',
    'int8',
    'serial8',
    'varbit',
    'int',
    'int4',
    'decimal',
    'float4',
    'int2',
    'serial2',
    'serial4',
  ],
  json: ['interval', 'json', 'jsonb', 'xml'],
  float: ['float8'],
};

export const consoleScalars = Object.values(consoleDataTypeToSQLTypeMap).flat();

export function adaptSQLDataType(
  sqlDataType: string
): TableColumn['consoleDataType'] {
  const [dataType] = getEntries(consoleDataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType)
  ) ?? ['string', []];

  return dataType;
}

export const adaptIntrospectedTables = ([
  fullTableListResult,
  partitionsTablesResult,
]: RunSQLResponse[]): IntrospectedTable[] => {
  const partitionNames =
    partitionsTablesResult.result?.map(row => row[0]) ?? [];
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
