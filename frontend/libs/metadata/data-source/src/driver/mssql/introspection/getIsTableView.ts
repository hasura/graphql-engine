import { runSQL } from '@hasura/metadata/api';
import { GetIsTableViewProps } from '../../types';
import type { MssqlTable } from '../types';

export const getIsTableView = async ({
  dataSourceName,
  table,
  fetchJson,
  endpoints,
}: GetIsTableViewProps) => {
  const { schema, name } = table as MssqlTable;

  const sql = `
    SELECT TABLE_NAME
    FROM information_schema.views
    WHERE TABLE_SCHEMA = '${schema}'
      AND TABLE_NAME = '${name}';`;

  const views = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'postgres',
      },
      sql: sql,
      readOnly: true,
    },
    url: endpoints.queryV2,
    fetchJson,
  });

  if (Array.isArray(views?.result)) return views?.result?.length > 1;
  return false;
};
