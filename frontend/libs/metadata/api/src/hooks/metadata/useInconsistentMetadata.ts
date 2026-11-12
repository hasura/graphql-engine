import { useQuery, useQueryClient } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { requestJson } from '@hasura/shared/utils';
import { INCONSISTENT_METADATA_QUERY_KEY } from '../constants';
import type { InconsistentMetadata } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';

const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

export const getInconsistentMetadataQuery = {
  type: 'get_inconsistent_metadata',
  args: {},
};

export const inconsistentSourcesSelector = (m: InconsistentMetadata) => {
  return m.inconsistent_objects.filter(
    (inconsistentObject) =>
      'type' in inconsistentObject && inconsistentObject.type === 'source',
  );
};

export const useInvalidateInconsistentMetadata = () => {
  const queryClient = useQueryClient();
  const invalidate = () =>
    queryClient.invalidateQueries({
      queryKey: [INCONSISTENT_METADATA_QUERY_KEY],
    });

  return invalidate;
};

export const useFetchInconsistentMetadata = () => {
  const { endpoints } = useAppContext();
  // This imperative fetch validates credentials during login/initialization.
  // Never reuse cached data or an in-flight query from another credential set.
  // The UI's useInconsistentMetadata query below remains cached.
  return (headers: Record<string, string>) =>
    requestJson<InconsistentMetadata>(endpoints.metadata, {
      method: 'POST',
      headers,
      body: JSON.stringify(getInconsistentMetadataQuery),
    });
};

export const useInconsistentMetadata = <T = InconsistentMetadata>(
  selector?: (m: InconsistentMetadata) => T,
  staleTime: number = DEFAULT_STALE_TIME,
  enabled = true,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery({
    queryKey: [INCONSISTENT_METADATA_QUERY_KEY],
    queryFn: () => {
      return fetchJson<InconsistentMetadata>(endpoints.metadata, {
        method: 'POST',
        body: JSON.stringify(getInconsistentMetadataQuery),
      });
    },
    staleTime: staleTime || DEFAULT_STALE_TIME,
    refetchOnWindowFocus: false,
    select: selector,
    enabled,
  });

  return queryReturn;
};
