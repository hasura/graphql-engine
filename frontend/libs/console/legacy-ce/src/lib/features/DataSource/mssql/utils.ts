import { TableColumn } from '../types';

export function adaptSQLDataType(sqlDataType: string): TableColumn['dataType'] {
  const DataTypeToSQLTypeMap: Record<TableColumn['dataType'], string[]> = {
    bool: [],
    string: ['char', 'text', 'varchar'],
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
    datetime: [
      'date',
      'datetime',
      'datetime2',
      'datetimeoffset',
      'smalldatetime',
      'time',
    ],
    timestamp: [],
    xml: ['xml'],
    json: [],
  };

  const [dataType] = Object.entries(DataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType)
  ) ?? ['string', []];

  return dataType as TableColumn['dataType'];
}
