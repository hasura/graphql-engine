import Endpoints from '@/Endpoints';
import { useAppSelector } from '@/store';
import React from 'react';
import { useQuery, UseQueryOptions } from 'react-query';
import { APIError } from './error';
import { Api } from './apiUtils';

export interface ServerConfig {
  version: string;
  is_function_permissions_inferred: boolean;
  is_admin_secret_set: boolean;
  is_auth_hook_set: boolean;
  is_remote_schema_permissions_enabled: boolean;
  is_jwt_set: boolean;
  experimental_features: string[];
  jwt: {
    claims_namespace: string;
    claims_format: string;
  };
}

export function useServerConfig<T = ServerConfig>(
  transformFn?: (d: ServerConfig) => T,
  queryOptions?: Omit<
    UseQueryOptions<ServerConfig, APIError, T, 'serverConfig'>,
    'queryKey' | 'queryFn'
  >
) {
  const headers = useAppSelector(s => s.tables.dataHeaders);

  // Hooks warning disabled cos of: https://tkdodo.eu/blog/react-query-data-transformations
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const select = React.useCallback(
    transformFn || ((d: ServerConfig) => (d as unknown) as T),
    []
  );

  return useQuery(
    'serverConfig',
    () => {
      return Api.get<ServerConfig>({ url: Endpoints.serverConfig, headers });
    },
    { ...queryOptions, select }
  );
}
