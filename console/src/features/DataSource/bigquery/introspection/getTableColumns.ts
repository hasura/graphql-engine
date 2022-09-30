import { getTableSchemaName, BrowseRowsTable } from '@/features/BrowseRows';
import { runSQL } from '../../api';
import { adaptTableColumns } from '../../common/utils';
import { GetTableColumnsProps } from '../../types';

export const getTableColumns = async ({
  dataSourceName,
  table,
  httpClient,
}: GetTableColumnsProps) => {
  const innerTable = table as BrowseRowsTable;

  if ('schema' in innerTable) {
    console.warn(
      'BigQuery: received key "schema" while expecting a table object with "dataset"',
      innerTable
    );
  }

  const dataset = getTableSchemaName(innerTable);
  if (!dataset) {
    return Promise.resolve([]);
  }

  const sql = `SELECT column_name, data_type FROM ${dataset}.INFORMATION_SCHEMA.COLUMNS WHERE table_name = '${innerTable?.name}';`;

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
