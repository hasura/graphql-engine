import { TableColumn } from '../types';

export const DataTypeToSQLTypeMap: Record<
  TableColumn['consoleDataType'],
  string[]
> = {
  // GraphQL exposes `bit` as a Boolean
  boolean: ['bit'],
  // `timestamp` is SQL Server's rowversion, a binary counter, not a date
  string: ['char', 'nchar', 'varchar', 'nvarchar', 'sysname', 'timestamp'],
  number: [],
  integer: ['bigint', 'int', 'smallint', 'tinyint'],
  text: ['text', 'ntext'],
  json: [],
  float: ['float', 'decimal', 'money', 'numeric', 'smallmoney', 'real'],
  timestamp: ['datetime', 'datetime2', 'smalldatetime', 'datetimeoffset'],
  date: ['date'],
  time: ['time'],
  geography: ['geography'],
  uuid: ['uniqueidentifier'],
  binary: ['binary', 'varbinary', 'image'],
  array: [],
};

export const DataTypeScalars = Object.values(DataTypeToSQLTypeMap).flat();

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
