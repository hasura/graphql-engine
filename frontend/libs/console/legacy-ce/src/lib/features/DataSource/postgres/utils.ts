import { getEntries } from '../../../components/Services/Data/Common/tsUtils';
import { TableColumn } from '../types';

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

export function adaptSQLDataType(
  sqlDataType: string
): TableColumn['consoleDataType'] {
  const [dataType] = getEntries(consoleDataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType)
  ) ?? ['string', []];

  return dataType;
}
