import Endpoints from '@/Endpoints';
import { HasuraMetadataV3 } from '@/metadata/types';
import { useAppSelector } from '@/store';
import React from 'react';
import { useQuery, UseQueryOptions } from 'react-query';
import { Api } from './apiUtils';

export interface MetadataResponse {
  resource_version: number;
  metadata: HasuraMetadataV3;
}

export function useMetadata<T = MetadataResponse>(
  transformFn?: (d: MetadataResponse) => T,
  queryOptions?: Omit<
    UseQueryOptions<MetadataResponse, Error, T, 'metadata'>,
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

  // Hooks warning disabled cos of: https://tkdodo.eu/blog/react-query-data-transformations
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const select = React.useCallback(
    transformFn || ((d: MetadataResponse) => (d as unknown) as T),
    []
  );

  return useQuery({
    queryKey: 'metadata',
    queryFn,
    ...queryOptions,
    select,
  });
}
