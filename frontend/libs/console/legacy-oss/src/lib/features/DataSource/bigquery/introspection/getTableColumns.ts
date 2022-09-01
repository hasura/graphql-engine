import { BigQueryTable } from '..';
import { runSQL } from '../../api';
import { adaptTableColumns } from '../../common/utils';
import { GetTableColumnsProps } from '../../types';

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
