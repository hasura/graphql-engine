import { getEntries } from '../../../../components/Services/Data/Common/tsUtils';
import { RunSQLResponse } from '../../api';
import { IntrospectedTable, TableColumn } from '../../types';

export const adaptIntrospectedBigQueryTables = (
  runSqlResponse: RunSQLResponse
): IntrospectedTable[] => {
  /*
    The `slice(1)` on the result is done because the first item of the result is always the columns names from the SQL output.
    It is not required for the final result and should be avoided
  */
  const adaptedResponse = runSqlResponse.result
    ?.slice(1)
    .map((row: string[]) => ({
      name: `${row[1]}.${row[0]}`,
      table: {
        name: row[0],
        dataset: row[1],
      },
      type: row[2],
    }));

  return adaptedResponse ?? [];
};

export function adaptSQLDataType(
  sqlDataType: string
): TableColumn['consoleDataType'] {
  const DataTypeToSQLTypeMap: Record<TableColumn['consoleDataType'], string[]> =
    {
      boolean: ['BOOL'],
      text: [],
      string: [
        'STRING',
        'STRUCT',
        'varchar',
        'DATE',
        'DATETIME',
        'TIME',
        'TIMESTAMP',
      ],
      number: ['BIGNUMERIC', 'FLOAT64', 'INT64', 'INTERVAL', 'NUMERIC'],
      json: ['JSON', 'xml'],
      float: ['FLOAT64'],
    };

  const [dataType] = getEntries(DataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(sqlDataType)
  ) ?? ['string', []];

  return dataType;
}
