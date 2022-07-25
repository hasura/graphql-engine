import { getRunSqlType } from '@/components/Common/utils/v1QueryUtils';
import { baseUrl as baseURL } from '@/Endpoints';
import globals from '@/Globals';
import axios from 'axios';
import { Metadata, Source } from './types';

export const httpClient = axios.create({
  baseURL,
  headers: {
    'x-hasura-admin-secret': globals.adminSecret ?? '',
  },
});

export const exportMetadata = async (): Promise<Metadata> =>
  (
    await httpClient.post('v1/metadata', {
      type: 'export_metadata',
      args: {},
    })
  ).data;

type RunSqlArgs = {
  source: Omit<Source, 'tables'>;
  sql: string;
};

export type RunSQLResponse =
  | {
      result: string[][];
      result_type: 'TuplesOk';
    }
  | {
      result_type: 'CommandOk';
      result: null;
    };

export const runSQL = async ({
  source,
  sql,
}: RunSqlArgs): Promise<RunSQLResponse> => {
  if (source.kind === 'gdc') throw Error('GDC does not support run sql');

  const type = getRunSqlType(source.kind);
  const result = await httpClient.post<RunSQLResponse>('v1/query', {
    type,
    args: {
      sql,
      source: source.name,
    },
  });
  return result.data;
};
