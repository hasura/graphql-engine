import { TableColumn } from '../types';

export function adaptSQLDataType(sqlDataType: string): TableColumn['dataType'] {
  const DataTypeToSQLTypeMap: Record<TableColumn['dataType'], string[]> = {
    bool: ['boolean', 'bool'],
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
      'text',
      'tsquery',
      'tsvector',
      'txid_snapshot',
      'uuid',
      'char',
      'varchar',
    ],
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
      'float8',
      'int',
      'int4',
      'decimal',
      'float4',
      'int2',
      'serial2',
      'serial4',
    ],
    datetime: ['date', 'time', 'time', 'timetz'],
    timestamp: ['timestamptz', 'timestamp'],
    xml: ['xml'],
    json: ['interval', 'json', 'jsonb'],
  };

  const [dataType] = Object.entries(DataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType)
  ) ?? ['string', []];

  return dataType as TableColumn['dataType'];
}
