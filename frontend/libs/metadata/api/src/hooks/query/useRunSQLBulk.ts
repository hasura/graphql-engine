import { useQuery } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { UseQueryOptions } from '@tanstack/react-query';
import { HttpError } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { runSQLBulk, RunSQLBulkProps } from '../../api';
import { RunSQLResponse } from '@hasura/shared/types';

export type UseRunSQLBulkOptions<K = RunSQLResponse[]> = Omit<
  UseQueryOptions<RunSQLResponse[], HttpError, K>,
  'queryFn'
>;

export function useRunSQLBulk<K = RunSQLResponse[]>(
  props: RunSQLBulkProps,
  options: UseRunSQLBulkOptions<K>,
) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    ...options,
    queryKey: options.queryKey,
    queryFn: () => {
      return runSQLBulk({
        ...props,
        url: endpoints.queryV2,
        fetchJson,
      });
    },
  });
}
