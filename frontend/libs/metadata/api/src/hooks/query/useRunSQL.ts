import {
  useMutation,
  UseMutationOptions,
  useQuery,
} from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { UseQueryOptions } from '@tanstack/react-query';
import { HttpError } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { runSQL, RunSqlArgs } from '../../api';
import {
  RunSQLCommandResponse,
  RunSQLSelectResponse,
} from '@hasura/shared/types';

export type UseRunSQLQueryOptions<K = RunSQLSelectResponse> = Omit<
  UseQueryOptions<RunSQLSelectResponse, HttpError, K>,
  'queryFn'
>;

export function useRunSQLQuery<K = RunSQLSelectResponse>(
  args: RunSqlArgs,
  options: UseRunSQLQueryOptions<K>,
) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    ...options,
    queryKey: options.queryKey,
    queryFn: () => {
      return runSQL({
        args,
        url: endpoints.queryV2,
        fetchJson,
      }) as Promise<RunSQLSelectResponse>;
    },
  });
}

export type UseRunSQLCommandOptions = Omit<
  UseMutationOptions<RunSQLCommandResponse, HttpError, RunSqlArgs>,
  'mutationFn'
>;

export function useRunSQLCommand(options?: UseRunSQLCommandOptions) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useMutation({
    ...options,
    mutationFn: (args: RunSqlArgs) => {
      return runSQL({
        args,
        url: endpoints.queryV2,
        fetchJson,
      }) as Promise<RunSQLCommandResponse>;
    },
  });
}
