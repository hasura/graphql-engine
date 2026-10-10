import { runSQL } from '@hasura/metadata/api';
import { GetVersionProps } from '../../types';

export const getVersion = async ({
  dataSourceName,
  endpoints,
  fetchJson,
}: GetVersionProps) => {
  const result = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'mssql',
      },
      sql: `SELECT @@VERSION as version;`,
    },
    fetchJson,
    url: endpoints.queryV2,
  });
  return result.result?.[1][0] ?? '';
};
