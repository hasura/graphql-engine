import { MssqlTable } from '..';
import { runSQL } from '../../api';
import { adaptTableColumns } from '../../common/utils';
import { GetTableColumnsProps } from '../../types';

export const getTableColumns = async ({
  dataSourceName,
  table,
  httpClient,
}: GetTableColumnsProps) => {
  const { schema, name } = table as MssqlTable;

  const sql = `SELECT COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'${name}' AND TABLE_SCHEMA= N'${schema}'`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'mssql',
    },
    sql,
    httpClient,
  });

  return adaptTableColumns(tables.result);
};
