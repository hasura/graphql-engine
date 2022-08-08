import { BigQueryTable } from '..';
import { runSQL } from '../../api';
import { adaptTableColumns } from '../../common/utils';

export const getTableColumns = async (
  dataSourceName: string,
  table: unknown
) => {
  const { dataset, name } = table as BigQueryTable;

  const sql = `SELECT column_name, data_type FROM ${dataset}.INFORMATION_SCHEMA.COLUMNS WHERE table_name = '${name}';`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'bigquery',
    },
    sql,
  });

  return adaptTableColumns(tables.result);
};
