import { QueryKey, useQuery, UseQueryOptions } from '@tanstack/react-query';
import { SupportedDriver } from '@hasura/shared/types';
import { getDatabaseMethods, TableColumnTypeMap } from '../../driver';
import { HttpError } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { useAuthFetchJson } from '@hasura/shared/hooks';

const GET_SUPPORTED_DATA_TYPES_QUERY_KEY = 'GET_SUPPORTED_DATA_TYPES';

type QueryOptions<T = TableColumnTypeMap> = Omit<
  UseQueryOptions<TableColumnTypeMap, HttpError, T>,
  'queryKey' | 'queryFn'
>;

export function useSupportedDataTypes<T = TableColumnTypeMap>(
  driver: SupportedDriver | null | undefined,
  options: QueryOptions<T> = {},
) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<TableColumnTypeMap, HttpError<any>, T, QueryKey>({
    queryKey: [GET_SUPPORTED_DATA_TYPES_QUERY_KEY, driver],
    queryFn: async () => {
      const sourceAPI = getDatabaseMethods(driver!);

      if (!driver) {
        return {} as TableColumnTypeMap;
      }

      return sourceAPI.introspection.getSupportedDataTypes({
        endpoints,
        fetchJson,
        driver,
      });
    },
    refetchOnWindowFocus: false,
    staleTime: Infinity,
    ...options,
    enabled: Boolean(driver) || options.enabled !== false,
  });
}
