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
