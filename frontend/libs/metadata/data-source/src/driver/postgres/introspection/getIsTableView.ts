import { runSQL } from '@hasura/metadata/api';
import { GetIsTableViewProps } from '../../types';
import { PostgresTable } from '../types';
import { PostgresFamilyDriver } from '@hasura/shared/types';

export const getIsTableViewCurry =
  (kind: PostgresFamilyDriver) =>
  async ({
    dataSourceName,
    table,
    fetchJson,
    endpoints,
  }: GetIsTableViewProps) => {
    const { schema, name } = table as PostgresTable;

    const sql = `
    SELECT TABLE_NAME
    FROM information_schema.views
    WHERE TABLE_SCHEMA = '${schema}'
      AND TABLE_NAME = '${name}';`;

    const views = await runSQL({
      args: {
        source: {
          name: dataSourceName,
          kind,
        },
        sql: sql,
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });

    if (Array.isArray(views?.result)) return views?.result?.length > 1;
    return false;
  };

export const getIsTableView = getIsTableViewCurry('postgres');
