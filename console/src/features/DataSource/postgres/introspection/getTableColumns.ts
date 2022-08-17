import { PostgresTable } from '..';
import { runSQL } from '../../api';
import { adaptTableColumns } from '../../common/utils';
import { GetTableColumnsProps } from '../../types';

export const getTableColumns = async ({
  dataSourceName,
  table,
  httpClient,
}: GetTableColumnsProps) => {
  const { schema, name } = table as PostgresTable;

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
      kind: 'postgres',
    },
    sql,
    httpClient,
  });

  return adaptTableColumns(tables.result);
};
