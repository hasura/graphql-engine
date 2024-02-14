import { CockroachDBTable } from '..';
import { runSQL, RunSQLResponse } from '../../api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType } from '../utils';

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map(row => ({
    name: row[0],
    consoleDataType: adaptSQLDataType(row[1]),
    dataType: row[1],
    nullable: row[2] === 'YES',
  }));
};

export const getTableColumns = async ({
  dataSourceName,
  table,
  httpClient,
}: GetTableColumnsProps) => {
  const { name, schema } = table as CockroachDBTable;

  const sql = `
  SELECT
   column_name, data_type
  FROM
    information_schema.columns
  WHERE
    table_schema = '${schema}' AND
    table_name  = '${name}';`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'cockroach',
    },
    sql,
    readOnly: true,
    httpClient,
  });

  return adaptTableColumns(tables.result);
};
