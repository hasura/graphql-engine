import { useCallback } from 'react';
import { useQuery } from '@tanstack/react-query';
import { fetchTemplateDataQueryFn, getHttpErrorMessage } from '../utils';
import { staleTime } from '../../constants';
import { useRunSQLCommand } from '@hasura/metadata/api';

/**
 * Hook to install migration from a remote file containing sql migrations.
 * @returns A memoised function which can be called imperatively to apply the migrations
 */
export function useInstallMigration(
  dataSourceName: string,
  migrationFileUrl: string,
  onSuccessCb?: () => void,
  onErrorCb?: (errorMsg?: string) => void,
): { performMigration: () => void } | { performMigration: undefined } {
  // Fetch the migration to be applied from remote file, or return from react-query cache if present
  const {
    data: migrationSQL,
    isLoading,
    isError,
  } = useQuery({
    queryKey: [migrationFileUrl],
    queryFn: () => fetchTemplateDataQueryFn<string>(migrationFileUrl, {}),
    staleTime,
  });

  const mutation = useRunSQLCommand({
    onSuccess: onSuccessCb,
    onError: (error: Error) => {
      if (onErrorCb) {
        onErrorCb(getHttpErrorMessage(error) ?? 'Failed to apply migration');
      }
    },
  });

  // only do a 'run_sql' call if we have the migrations file data from the remote url.
  // otherwise `performMigration` will just return an empty function. In that case, error callbacks will have info on what went wrong.
  const performMigration = useCallback(() => {
    if (migrationSQL) {
      mutation.mutate({
        sql: migrationSQL,
        source: {
          name: dataSourceName,
          kind: 'postgres',
        },
      });
    }

    // not adding mutation to dependencies as its a non-memoised function, will trigger this useCallback
    // every time we do a mutation. https://github.com/TanStack/query/issues/1858
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [dataSourceName, migrationSQL]);

  if (isError) {
    if (onErrorCb) {
      onErrorCb(
        `Failed to fetch migration data from the provided Url: ${migrationFileUrl}`,
      );
    }
    return { performMigration: undefined };
  }

  if (isLoading) {
    return { performMigration: undefined };
  }

  return { performMigration };
}
