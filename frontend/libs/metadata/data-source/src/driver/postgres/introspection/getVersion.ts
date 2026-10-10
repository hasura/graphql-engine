import { PostgresFamilyDriver } from '@hasura/shared/types';
import { GetVersionProps } from '../../types';
import { runSQL } from '@hasura/metadata/api';

export const getVersionCurry =
  (kind: PostgresFamilyDriver) =>
  async ({ dataSourceName, fetchJson, endpoints }: GetVersionProps) => {
    const result = await runSQL({
      args: {
        source: {
          name: dataSourceName,
          kind: kind,
        },
        sql: `SELECT VERSION()`,
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });

    return result.result?.[1][0] ?? '';
  };

export const getVersion = getVersionCurry('postgres');
