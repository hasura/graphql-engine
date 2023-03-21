import { getEntries } from '../../../components/Services/Data/Common/tsUtils';
import { TableColumn } from '../types';

export function adaptSQLDataType(
  sqlDataType: string
): TableColumn['consoleDataType'] {
  const DataTypeToSQLTypeMap: Record<TableColumn['consoleDataType'], string[]> =
    {
      boolean: [],
      string: ['char', 'boolean', 'text', 'varchar'],
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

  const [dataType] = getEntries(DataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType)
  ) ?? ['string', []];

  return dataType;
}
