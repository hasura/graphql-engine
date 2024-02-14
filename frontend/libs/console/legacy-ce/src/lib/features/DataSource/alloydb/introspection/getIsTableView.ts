import { runSQL } from '../../api';
import { GetIsTableViewProps } from '../../types';
import { AlloyDbTable } from '../index';

export const getIsTableView = async ({
  dataSourceName,
  table,
  httpClient,
}: GetIsTableViewProps) => {
  const { schema, name } = table as AlloyDbTable;

  const sql = `
    SELECT TABLE_NAME
    FROM information_schema.views
    WHERE TABLE_SCHEMA = '${schema}'
      AND TABLE_NAME = '${name}';`;

  const views = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'postgres',
    },
    sql: sql,
    readOnly: true,
    httpClient,
  });

  if (Array.isArray(views?.result)) return views?.result?.length > 1;
  return false;
};
