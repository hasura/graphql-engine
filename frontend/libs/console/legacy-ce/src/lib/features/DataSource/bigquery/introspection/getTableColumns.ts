import { runSQL, RunSQLResponse } from '../../api';
import { BigQueryTable } from '..';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType } from './utils';

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map(row => ({
    name: row[0],
    dataType: adaptSQLDataType(row[1]),
    nullable: row[2] === 'YES',
  }));
};

export const getTableColumns = async ({
  dataSourceName,
  table,
  httpClient,
}: GetTableColumnsProps) => {
  const { dataset, name } = table as BigQueryTable;

  const sql = `SELECT column_name, data_type FROM ${dataset}.INFORMATION_SCHEMA.COLUMNS WHERE table_name = '${name}';`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'bigquery',
    },
    sql,
    httpClient,
  });

  return adaptTableColumns(tables.result);
};
