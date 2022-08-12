import { CitusTable } from '..';
import { runSQL } from '../../api';
import { adaptTableColumns } from '../../common/utils';

export const getTableColumns = async (
  dataSourceName: string,
  table: unknown
) => {
  const { name, schema } = table as CitusTable;

  const sql = `
  SELECT 
   column_name, data_type 
  FROM 
    information_schema.columns 
  WHERE 
    table_schema = '${name}' AND 
    table_name  = '${schema}';`;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'citus',
    },
    sql,
  });

  return adaptTableColumns(tables.result);
};
