import { Metadata } from '@/features/hasura-metadata-types';
import { rest } from 'msw';

const mockMetadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [],
        configuration: {
          connection_info: {
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'repeatable-read',
            pool_settings: {
              connection_lifetime: 150,
              idle_timeout: 150,
              pool_timeout: 150,
              retries: 150,
              total_max_connections: 150,
            },
            use_prepared_statements: true,
          },
          extensions_schema: 'test_public',
          read_replicas: [
            {
              database_url:
                'postgres://postgres:test@host.docker.internal:6001/chinook',
              isolation_level: 'read-committed',
              use_prepared_statements: false,
            },
          ],
        },
        customization: {
          root_fields: {
            namespace: 'namespace_',
            prefix: 'prefix_',
            suffix: '_suffix',
          },
          type_names: {
            prefix: 'prefix_',
            suffix: '_suffix',
          },
        },
      },
      {
        name: 'mssql1',
        kind: 'mssql',
        tables: [],
        configuration: {
          connection_info: {
            connection_string: {
              from_env: 'HASURA_ENV_VAR',
            },
            pool_settings: {
              max_connections: 50,
              idle_timeout: 180,
            },
          },
        },
        customization: {
          root_fields: {
            namespace: 'some_field_name',
            prefix: 'some_field_name_prefix',
            suffix: 'some_field_name_suffix',
          },
          type_names: {
            prefix: 'some_type_name_prefix',
            suffix: 'some_type_name_suffix',
          },
        },
      },
    ],
  },
};

export const handlers = () => [
  rest.post('http://localhost:8080/v1/metadata', (req, res, ctx) => {
    const requestBody = req.body as Record<string, any>;

    if (requestBody.type === 'export_metadata')
      return res(ctx.json(mockMetadata));

    return res(ctx.json({}));
  }),
  rest.get(`http://localhost:8080/v1alpha1/config`, (req, res, ctx) => {
    return res(
      ctx.json({
        version: 'dev-fb2bab3-test-app',
        is_function_permissions_inferred: true,
        is_remote_schema_permissions_enabled: true,
        is_admin_secret_set: false,
        is_auth_hook_set: false,
        is_jwt_set: false,
        jwt: [],
        is_allow_list_enabled: false,
        live_queries: { batch_size: 100, refetch_delay: 1 },
        streaming_queries: { batch_size: 100, refetch_delay: 1 },
        console_assets_dir: null,
        experimental_features: ['naming_convention'],
        is_prometheus_metrics_enabled: false,
        default_naming_convention: null,
      })
    );
  }),
];
