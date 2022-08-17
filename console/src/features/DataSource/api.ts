import { getRunSqlType } from '@/components/Common/utils/v1QueryUtils';
import { AxiosInstance } from 'axios';
import { Metadata, Source } from './types';

export interface NetworkArgs {
  httpClient: AxiosInstance;
}

export const exportMetadata = async ({
  httpClient,
}: NetworkArgs): Promise<Metadata> => {
  return (
    await httpClient.post('/v1/metadata', {
      type: 'export_metadata',
      args: {},
    })
  ).data;
};

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
  httpClient,
}: RunSqlArgs & NetworkArgs): Promise<RunSQLResponse> => {
  if (source.kind === 'gdc') throw Error('GDC does not support run sql');

  const type = getRunSqlType(source.kind);
  /**
   * Use v2 query instead of v1 because it supports other <db>_run_sql commands
   */
  const result = await httpClient.post<RunSQLResponse>('v2/query', {
    type,
    args: {
      sql,
      source: source.name,
    },
  });
  return result.data;
};
