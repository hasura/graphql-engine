import type { ServerConfig } from '../../../../../../src/hooks/useServerConfig';

export const config: ServerConfig = {
  version: 'v2.8.1',
  is_function_permissions_inferred: true,
  default_naming_convention: '',
  is_remote_schema_permissions_enabled: true,
  is_admin_secret_set: false,
  is_auth_hook_set: false,
  is_jwt_set: false,
  is_allow_list_enabled: false,
  experimental_features: [],
  jwt: {
    claims_namespace: '',
    claims_format: '',
  },
  is_prometheus_metrics_enabled: false
};
