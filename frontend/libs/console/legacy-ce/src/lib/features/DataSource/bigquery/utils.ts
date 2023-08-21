import { Capabilities } from '@hasura/dc-api-types';
import { TableColumn } from '../types';

export const DataTypeToSQLTypeMap: Record<
  TableColumn['consoleDataType'],
  string[]
> = {
  boolean: [],
  string: ['STRING', 'DATETIME', 'TIME', 'TIMESTAMP', 'DATE'],
  number: [
    'INT64',
    'NUMERIC',
    'DECIMAL',
    'BIGNUMERIC',
    'BIGDECIMAL',
    'FLOAT64',
  ],
  text: [],
  json: [],
  float: [],
};

export const DataTypeScalars = Object.values(DataTypeToSQLTypeMap).flat();

export const bigQueryCapabilities: Capabilities = {
  queries: {
    foreach: {},
  },
  relationships: {},
  data_schema: {
    supports_foreign_keys: false,
  },
  interpolated_queries: {},
};
