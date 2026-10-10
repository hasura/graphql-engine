import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { HttpError } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { NamingConvention } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';

export type ExperimentalFeature =
  'streaming_subscriptions' | 'naming_convention' | 'apollo_federation';

export type FeatureFlag = {
  name: string;
  description: string;
  enabled: boolean;
};

export interface ServerConfig {
  version: string;
  is_function_permissions_inferred: boolean;
  default_naming_convention: NamingConvention;
  is_admin_secret_set: boolean;
  is_auth_hook_set: boolean;
  is_allow_list_enabled: boolean;
  is_remote_schema_permissions_enabled: boolean;
  is_jwt_set: boolean;
  experimental_features: ExperimentalFeature[];
  feature_flags: FeatureFlag[];
  jwt: {
    claims_namespace: string;
    claims_format: string;
  };
  is_prometheus_metrics_enabled: boolean;
  is_apollo_federation_enabled: boolean;
}

const options = {
  method: 'GET',
};

export function useServerConfig<T = ServerConfig>(
  transformFn?: (d: ServerConfig) => T,
  queryOptions?: Omit<
    UseQueryOptions<ServerConfig, HttpError, T, ['serverConfig']>,
    'queryKey' | 'queryFn'
  >,
) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const select = transformFn || ((d: ServerConfig) => d as unknown as T);

  return useQuery({
    queryKey: ['serverConfig'],
    queryFn: async () => {
      const result = await fetchJson<ServerConfig>(
        endpoints.serverConfig,
        options,
      );

      return result;
    },
    ...queryOptions,
    select,
    refetchOnWindowFocus: false,
    // server config is never changed in runtime.
    staleTime: Infinity,
  });
}
