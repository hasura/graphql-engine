import Endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { useAppSelector } from '@/store';
import { useQuery, UseQueryOptions } from 'react-query';
import type { MetadataResponse } from '../types';

export function useMetadata<T extends (d: MetadataResponse) => any>(
  select: T = (((d: MetadataResponse) => d) as unknown) as T,
  queryOptions?: Omit<
    UseQueryOptions<MetadataResponse, Error, ReturnType<T>, 'metadata'>,
    'queryKey' | 'queryFn'
  >
) {
  const body = {
    type: 'export_metadata',
    version: 2,
    args: {},
  };

  const headers = useAppSelector(state => state.tables.dataHeaders);
  const queryFn = () => {
    return Api.post<MetadataResponse>({
      headers,
      body,
      url: Endpoints.metadata,
    });
  };

  return useQuery({
    queryKey: 'metadata',
    queryFn,
    ...queryOptions,
    select,
  });
}
