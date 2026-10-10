import { TableColumn } from '../types';

export const DataTypeToSQLTypeMap: Record<
  TableColumn['consoleDataType'],
  string[]
> = {
  boolean: [],
  string: ['char', 'text', 'varchar', 'nvarchar', 'sysname'],
  number: [],
  integer: ['bigint', 'bit', 'int', 'smallint'],
  text: ['text'],
  json: [],
  float: ['float', 'decimal', 'money', 'numeric', 'smallmoney', 'real'],
  timestamp: ['datetime', 'smalldatetime', 'datetimeoffset', 'timestamp'],
  date: ['date'],
  time: ['time'],
  geography: [],
  uuid: [],
  array: [],
};

export const DataTypeScalars = Object.values(DataTypeToSQLTypeMap).flat();

export const columnDataTypes = {
  BIGINT: 'bigint',
  BIT: 'bit',
  DECIMAL: 'decimal',
  INT: 'int',
  MONEY: 'money',
  FLOAT: 'float',
  NCHAR: 'nchar',
  NTEXT: 'ntext',
  NVARCHAR: 'nvarchar',
  BINARY: 'binary',
  IMAGE: 'image',
  NUMERIC: 'numeric',
  DATE: 'date',
  DATETIME: 'datetime',
  DATETIME2: 'datetime2',
};

export function adaptSQLDataType(
  sqlDataType: string,
): TableColumn['consoleDataType'] {
  const [dataType] = Object.entries(DataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType),
  ) ?? ['string', []];

  return dataType as TableColumn['consoleDataType'];
}

export const isMSSQLTable = (tableType: string) => {
  return tableType === 'TABLE' || tableType === 'BASE TABLE';
};
