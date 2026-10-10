import { QueryKey, useQuery, UseQueryOptions } from '@tanstack/react-query';
import { SupportedDriver } from '@hasura/shared/types';
import { getDatabaseMethods } from '../../driver';
import { HttpError } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';

const GET_SUPPORTED_SCALARS_QUERY_KEY = 'GET_SUPPORTED_SCALARS';

type QueryOptions<T = string[]> = Omit<
  UseQueryOptions<string[], HttpError, T>,
  'queryKey' | 'queryFn'
>;

export function useSupportedScalars<T = string[]>(
  driver: SupportedDriver | null | undefined,
  options: QueryOptions<T> = {},
) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<string[], HttpError, T, QueryKey>({
    queryKey: [GET_SUPPORTED_SCALARS_QUERY_KEY, driver],
    queryFn: async () => {
      const sourceAPI = getDatabaseMethods(driver!);
      if (!driver) {
        return [];
      }

      return sourceAPI.introspection.getSupportedScalars({
        driver,
        endpoints,
        fetchJson,
      });
    },
    refetchOnWindowFocus: false,
    staleTime: Infinity,
    ...options,
    enabled: Boolean(driver) || options.enabled !== false,
  });
}
