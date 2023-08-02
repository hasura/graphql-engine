export default {
  version: '12345',
  is_function_permissions_inferred: true,
  is_remote_schema_permissions_enabled: false,
  is_admin_secret_set: true,
  is_auth_hook_set: false,
  is_jwt_set: false,
  jwt: [],
  is_allow_list_enabled: false,
  live_queries: { batch_size: 100, refetch_delay: 1 },
  streaming_queries: { batch_size: 100, refetch_delay: 1 },
  console_assets_dir: null,
  experimental_features: ['naming_convention'],
  is_prometheus_metrics_enabled: false,
  default_naming_convention: 'hasura-default',
  feature_flags: [
    {
      name: 'stored-procedures',
      description: 'Expose stored procedures support',
      enabled: false,
    },
    {
      name: 'native-query-interface',
      description:
        'Expose custom views, permissions and advanced SQL functionality via custom queries',
      enabled: true,
    },
    {
      name: 'test-flag',
      description: 'Testing feature flag integration',
      enabled: false,
    },
  ],
};
