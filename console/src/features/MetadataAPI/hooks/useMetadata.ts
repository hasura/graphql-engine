import Endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { useAppSelector } from '@/store';
import { useQuery, UseQueryOptions, UseQueryResult } from 'react-query';
import type { MetadataResponse } from '../types';

// overloads
export function useMetadata(): UseQueryResult<MetadataResponse, Error>;
export function useMetadata<T extends (d: MetadataResponse) => any>(
  select: T
): UseQueryResult<ReturnType<T>, Error>;
export function useMetadata<
  T extends (d: MetadataResponse) => any,
  D extends (d: ReturnType<T>) => any
>(
  select: T,
  transformFn: D,
  queryOptions?: Omit<
    UseQueryOptions<MetadataResponse, Error, ReturnType<T>, 'metadata'>,
    'queryKey' | 'queryFn'
  >
): UseQueryResult<ReturnType<D>, Error>;

export function useMetadata(
  select = (d: MetadataResponse) => d,
  transformFn = (d: unknown) => d,
  queryOptions?: Omit<
    UseQueryOptions<MetadataResponse, Error, unknown, 'metadata'>,
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
    select: d => transformFn(select(d)),
  });
}
