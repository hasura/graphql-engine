import { PostgresTable } from '..';
import { runSQL } from '../../api';
import { adaptTableColumns } from '../../common/utils';

export const getTableColumns = async (
  dataSourceName: string,
  table: unknown
) => {
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
  });

  return adaptTableColumns(tables.result);
};
