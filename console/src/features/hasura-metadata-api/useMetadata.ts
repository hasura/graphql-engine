import { exportMetadata } from '@/features/DataSource';
import { Metadata } from '@/features/hasura-metadata-types';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';

export const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

/* 
  See the ./metadata-hooks for examples of how to use this hook
  Use the selector arg to tell react-query which part(s) of the metadata you want
  Default stale time is 5 minutes, but can be adjusted using the staleTime arg
*/

export const useMetadata = <T = Metadata>(
  selector?: (m: Metadata) => T,
  staleTime: number = DEFAULT_STALE_TIME
) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['export_metadata'],
    queryFn: async () => {
      const result = await exportMetadata({ httpClient });
      return result;
    },
    staleTime: staleTime || DEFAULT_STALE_TIME,
    refetchOnWindowFocus: false,
    select: selector,
  });
};
