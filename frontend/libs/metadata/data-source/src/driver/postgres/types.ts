import { TableColumn } from '../types';

export type PostgresTable = { name: string; schema: string };
export type PostgresFunction = { name: string; schema: string };

export enum ArgType {
  CompositeType = 'c',
  BaseType = 'b',
}

export type PGInputArgType = {
  schema: string;
  name: string;
  type: ArgType;
};

export type TrackableComputedFunction = {
  function: PostgresFunction;
  definition: string;
  return_type_type: ArgType;
  return_type: PostgresFunction;
  input_arg_types?: PGInputArgType[];
  input_arg_names?: string[];
  returns_set: boolean;
};

export const postgresColumnDataTypes = {
  ARRAY: 'ARRAY',
  BIGINT: 'bigint',
  BIGSERIAL: 'bigserial',
  BOOLEAN: 'boolean',
  BOOL: 'bool',
  DATE: 'date',
  DATETIME: 'datetime',
  INTEGER: 'integer',
  JSONB: 'jsonb',
  JSONDTYPE: 'json',
  NUMERIC: 'numeric',
  SERIAL: 'serial',
  TEXT: 'text',
  TIME: 'time with time zone',
  TIMESTAMP: 'timestamp with time zone',
  TIMETZ: 'timetz',
  UUID: 'uuid',
};

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
    'char',
    'varchar',
    'bytea',
    'cidr',
    'inet',
  ],
  number: [],
  text: ['text', 'interval', 'xml'],
  integer: [
    'bigint',
    'bigserial',
    'bit',
    'bit varying',
    'integer',
    'smallint',
    'smallserial',
    'serial',
    'int8',
    'serial8',
    'varbit',
    'int',
    'int4',
    'int2',
    'serial2',
    'serial4',
  ],
  json: ['json', 'jsonb'],
  float: [
    'double precision',
    'money',
    'numeric',
    'real',
    'decimal',
    'float4',
    'float8',
  ],
  timestamp: [
    'timetz',
    'timestamptz',
    'timestamp',
    'timestamp with time zone',
    'timestamp without time zone',
    'time with time zone',
    'time without time zone',
  ],
  date: ['date'],
  time: ['time'],
  geography: ['geography'],
  uuid: ['uuid'],
  array: ['ARRAY'],
};

export const consoleScalars = Object.values(consoleDataTypeToSQLTypeMap).flat();
