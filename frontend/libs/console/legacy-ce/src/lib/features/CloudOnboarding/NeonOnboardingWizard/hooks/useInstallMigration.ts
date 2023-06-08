import { useCallback } from 'react';
import { getRunSqlQuery } from '../../../../components/Common/utils/v1QueryUtils';
import Endpoints from '../../../../Endpoints';
import { RunSQLResponse } from '../../../DataSource';
import { Api } from '../../../../hooks/apiUtils';
import { useAppSelector } from '../../../../storeHooks';
import { useMutation, useQuery } from 'react-query';
import { fetchTemplateDataQueryFn } from '../utils';
import { staleTime } from '../../constants';
import 'whatwg-fetch';

type MutationFnArgs = {
  sql: string;
  source: string;
  headers: Record<string, string>;
};

/**
 * Mutation Function to install the migration. Calls the `run_sql` api (assuming postgres driver)
 */
const installMigrationMutationFn = (args: MutationFnArgs) => {
  const { sql, source, headers } = args;
  const sqlPayload = getRunSqlQuery(sql, source);
  return Api.post<RunSQLResponse>({
    url: Endpoints.query,
    headers,
    body: sqlPayload,
  });
};

/**
 * Hook to install migration from a remote file containing sql migrations.
 * @returns A memoised function which can be called imperatively to apply the migrations
 */
export function useInstallMigration(
  dataSourceName: string,
  migrationFileUrl: string,
  onSuccessCb?: () => void,
  onErrorCb?: (errorMsg?: string) => void
): { performMigration: () => void } | { performMigration: undefined } {
  const headers = useAppSelector(state => state.tables.dataHeaders);

  // Fetch the migration to be applied from remote file, or return from react-query cache if present
  const {
    data: migrationSQL,
    isLoading,
    isError,
  } = useQuery(
    migrationFileUrl,
    () => fetchTemplateDataQueryFn<string>(migrationFileUrl, {}),
    {
      staleTime,
    }
  );

  const mutation = useMutation(
    (args: MutationFnArgs) => installMigrationMutationFn(args),
    {
      onSuccess: onSuccessCb,
      onError: (error: Error) => {
        if (onErrorCb) {
          onErrorCb(error.message ?? 'Failed to apply migration');
        }
      },
    }
  );

  // only do a 'run_sql' call if we have the migrations file data from the remote url.
  // otherwise `performMigration` will just return an empty function. In that case, error callbacks will have info on what went wrong.
  const performMigration = useCallback(() => {
    if (migrationSQL) {
      mutation.mutate({
        sql: migrationSQL,
        source: dataSourceName,
        headers,
      });
    }
    // not adding mutation to dependencies as its a non-memoised function, will trigger this useCallback
    // every time we do a mutation. https://github.com/TanStack/query/issues/1858
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [dataSourceName, migrationSQL, headers]);

  if (isError) {
    if (onErrorCb) {
      onErrorCb(
        `Failed to fetch migration data from the provided Url: ${migrationFileUrl}`
      );
    }
    return { performMigration: undefined };
  }

  if (isLoading) {
    return { performMigration: undefined };
  }

  return { performMigration };
}
