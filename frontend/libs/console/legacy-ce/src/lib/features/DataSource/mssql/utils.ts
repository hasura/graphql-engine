import { getEntries } from '../../../components/Services/Data/Common/tsUtils';
import { TableColumn } from '../types';

export const DataTypeToSQLTypeMap: Record<
  TableColumn['consoleDataType'],
  string[]
> = {
  boolean: [],
  string: ['char', 'text', 'varchar', 'nvarchar', 'sysname'],
  number: [
    'bigint',
    'bit',
    'decimal',
    'int',
    'money',
    'numeric',
    'smallint',
    'smallmoney',
    'float',
    'real',
  ],
  text: ['text'],
  json: [],
  float: ['float'],
};

export const DataTypeScalars = Object.values(DataTypeToSQLTypeMap).flat();

export function adaptSQLDataType(
  sqlDataType: string
): TableColumn['consoleDataType'] {
  const [dataType] = getEntries(DataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType)
  ) ?? ['string', []];

  return dataType;
}
