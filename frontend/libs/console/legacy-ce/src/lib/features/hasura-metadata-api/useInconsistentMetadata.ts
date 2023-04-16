import { useCallback } from 'react';
import { useQuery, useQueryClient } from 'react-query';
import { runMetadataQuery } from './runMetadataQuery';
import { useHttpClient } from '../Network';
import { InconsistentMetadata } from './types';

export const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

export const QUERY_KEY = 'inconsistent_objects';

export const useInvalidateInconsistentMetadata = () => {
  const queryClient = useQueryClient();
  const invalidate = useCallback(
    () => queryClient.invalidateQueries([QUERY_KEY]),
    [queryClient]
  );

  return invalidate;
};

export const useInconsistentMetadata = <T = InconsistentMetadata>(
  selector?: (m: InconsistentMetadata) => T,
  staleTime: number = DEFAULT_STALE_TIME
) => {
  const httpClient = useHttpClient();
  const invalidateInconsistentMetadata = useInvalidateInconsistentMetadata();

  const queryReturn = useQuery({
    queryKey: [QUERY_KEY],
    queryFn: async () => {
      const result = (await runMetadataQuery({
        httpClient,
        body: { type: 'get_inconsistent_metadata', args: {} },
      })) as InconsistentMetadata;
      return result;
    },
    staleTime: staleTime || DEFAULT_STALE_TIME,
    refetchOnWindowFocus: false,
    select: selector,
  });

  return {
    ...queryReturn,
    invalidateInconsistentMetadata,
  };
};
