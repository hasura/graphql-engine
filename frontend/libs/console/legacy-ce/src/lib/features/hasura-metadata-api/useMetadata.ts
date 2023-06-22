import { exportMetadata } from './exportMetadata';
import { Metadata } from '../hasura-metadata-types';
import { useHttpClient } from '../Network';
import { useCallback } from 'react';
import { useQuery, useQueryClient } from 'react-query';
import { APIError } from '../../hooks/error';
import { useAppDispatch } from '../../storeHooks';

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

type Options = {
  staleTime?: number;
  enabled?: boolean;
};

export const useMetadata = <FinalResult = Metadata>(
  selector?: (m: Metadata) => FinalResult,
  options: Options = {
    staleTime: DEFAULT_STALE_TIME,
    enabled: true,
  }
) => {
  const httpClient = useHttpClient();
  const invalidateMetadata = useInvalidateMetadata();
  const dispatch = useAppDispatch();

  const queryReturn = useQuery<Metadata, APIError, FinalResult>({
    queryKey: [METADATA_QUERY_KEY],
    queryFn: async () => {
      const result = await exportMetadata({ httpClient });

      dispatch({
        type: 'Metadata/EXPORT_METADATA_SUCCESS',
        data: result,
      });

      return result;
    },
    staleTime: options.staleTime,
    refetchOnWindowFocus: false,
    select: selector,
    enabled: options.enabled,
  });

  return {
    ...queryReturn,
    invalidateMetadata,
  };
};
