import { exportMetadata } from '../DataSource';
import { Metadata } from '../hasura-metadata-types';
import { useHttpClient } from '../Network';
import { useCallback } from 'react';
import { useQuery, useQueryClient } from 'react-query';

export const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

/*
  See the ./metadata-hooks for examples of how to use this hook
  Use the selector arg to tell react-query which part(s) of the metadata you want
  Default stale time is 5 minutes, but can be adjusted using the staleTime arg
*/

export const METADATA_QUERY_KEY = 'export_metadata';

export const useInvalidateMetadata = () => {
  const queryClient = useQueryClient();
  const invalidate = useCallback(
    () => queryClient.invalidateQueries([METADATA_QUERY_KEY]),
    [queryClient]
  );

  return invalidate;
};

export const useMetadata = <T = Metadata>(
  selector?: (m: Metadata) => T,
  staleTime: number = DEFAULT_STALE_TIME
) => {
  const httpClient = useHttpClient();
  const invalidateMetadata = useInvalidateMetadata();

  const queryReturn = useQuery({
    queryKey: [METADATA_QUERY_KEY],
    queryFn: async () => {
      const result = await exportMetadata({ httpClient });
      return result;
    },
    staleTime: staleTime || DEFAULT_STALE_TIME,
    refetchOnWindowFocus: false,
    select: selector,
  });

  return {
    ...queryReturn,
    invalidateMetadata,
  };
};
