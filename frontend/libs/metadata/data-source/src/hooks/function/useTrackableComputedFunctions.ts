import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import { getTrackableComputedFunctions } from '../../driver/postgres/introspection/getTrackableComputedFunctions';
import { TrackableComputedFunction } from '../../driver';

const GET_TRACKABLE_COMPUTED_FUNCTIONS_QUERY_KEY =
  'GET_TRACKABLE_COMPUTED_FUNCTIONS';

const useTrackableComputedFunctionsQueryKey = (dataSourceName: string) => {
  return [dataSourceName, GET_TRACKABLE_COMPUTED_FUNCTIONS_QUERY_KEY];
};

export const useTrackableComputedFunctions = (
  {
    source,
  }: {
    source: Partial<QualifiedDataSource>;
  },
  options?: UseQueryOptions<
    TrackableComputedFunction[],
    unknown,
    TrackableComputedFunction[]
  >,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: useTrackableComputedFunctionsQueryKey(source.name ?? ''),
    queryFn: async () => {
      if (!source.kind || !source.name) {
        return [];
      }

      return getTrackableComputedFunctions({
        dataSourceName: source.name,
        endpoints,
        fetchJson,
      });
    },
    ...options,
    enabled:
      Boolean(source?.name && source?.kind) && options?.enabled !== false,
  });
};
