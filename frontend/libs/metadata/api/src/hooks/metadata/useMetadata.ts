import { useQuery } from '@tanstack/react-query';
import type { Metadata } from '@hasura/shared/types';
import { exportMetadata } from '../../api/exportMetadata';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { HttpError } from '@hasura/shared/types';
import { METADATA_QUERY_KEY } from '../constants';
import { useAppContext } from '@hasura/shared/context';

const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

/*
  See the ./metadata-hooks for examples of how to use this hook
  Use the selector arg to tell react-query which part(s) of the metadata you want
  Default stale time is 5 minutes, but can be adjusted using the staleTime arg
*/

export type Options = {
  staleTime?: number;
  enabled?: boolean;
};

export const useMetadata = <FinalResult = Metadata>(
  selector?: (m: Metadata) => FinalResult,
  options: Options = {
    staleTime: DEFAULT_STALE_TIME,
    enabled: true,
  },
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const queryReturn = useQuery<Metadata, HttpError, FinalResult>({
    queryKey: [METADATA_QUERY_KEY],
    queryFn: async () => {
      const result = await exportMetadata({
        url: endpoints.metadata,
        fetchJson,
      });

      return result;
    },
    staleTime: options.staleTime,
    refetchOnWindowFocus: false,
    select: selector,
    enabled: options.enabled,
  });

  return queryReturn;
};
