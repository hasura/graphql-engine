import { Metadata } from '../../hasura-metadata-types';
import { rest } from 'msw';

const mockMetadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'sqlite_test',
        kind: 'sqlite',
        tables: [],
        configuration: {
          template: null,
          timeout: null,
          value: {
            db: './chinook.db',
            explicit_main_schema: false,
            include_sqlite_meta_tables: false,
            tables: ['Album', 'Artist', 'Genre', 'Track'],
          },
        },
      },
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

export const mockCapabilitiesResponse = {
  capabilities: {
    comparisons: { subquery: { supports_relations: true } },
    data_schema: { supports_foreign_keys: true, supports_primary_keys: true },
    explain: {},
    queries: {},
    raw: {},
    relationships: {},
    scalar_types: {
      DateTime: {
        comparison_operators: { _in_year: 'int' },
        graphql_type: 'String',
      },
      bool: {
        comparison_operators: {
          _and: 'bool',
          _nand: 'bool',
          _or: 'bool',
          _xor: 'bool',
        },
        graphql_type: 'Boolean',
      },
      decimal: {
        aggregate_functions: { max: 'decimal', min: 'decimal', sum: 'decimal' },
        comparison_operators: { _modulus_is_zero: 'decimal' },
        graphql_type: 'Float',
        update_column_operators: {
          dec: { argument_type: 'decimal' },
          inc: { argument_type: 'decimal' },
        },
      },
      number: {
        aggregate_functions: { max: 'number', min: 'number', sum: 'number' },
        comparison_operators: { _modulus_is_zero: 'number' },
        graphql_type: 'Float',
        update_column_operators: {
          dec: { argument_type: 'number' },
          inc: { argument_type: 'number' },
        },
      },
      string: {
        aggregate_functions: { max: 'string', min: 'string' },
        comparison_operators: { _glob: 'string', _like: 'string' },
        graphql_type: 'String',
      },
    },
  },
  config_schema_response: {
    config_schema: {
      nullable: false,
      properties: {
        DEBUG: {
          additionalProperties: true,
          description: 'For debugging.',
          nullable: true,
          type: 'object',
        },
        db: { description: 'The SQLite database file to use.', type: 'string' },
        explicit_main_schema: {
          default: false,
          description: "Prefix all tables with the 'main' schema",
          nullable: true,
          type: 'boolean',
        },
        include_sqlite_meta_tables: {
          description:
            'By default index tables, etc are not included, set this to true to include them.',
          nullable: true,
          type: 'boolean',
        },
        tables: {
          description:
            'List of tables to make available in the schema and for querying',
          items: { $ref: '#/other_schemas/TableName' },
          nullable: true,
          type: 'array',
        },
      },
      required: ['db'],
      type: 'object',
    },
    other_schemas: { TableName: { nullable: false, type: 'string' } },
  },
  display_name: 'Hasura SQLite',
  options: { uri: 'http://host.docker.internal:8100' },
};

export const handlers = () => [
  rest.post('http://localhost:8080/v1/metadata', (req, res, ctx) => {
    const requestBody = req.body as Record<string, any>;

    if (requestBody.type === 'export_metadata')
      return res(ctx.json(mockMetadata));

    if (requestBody.type === 'get_source_kind_capabilities')
      return res(ctx.json(mockCapabilitiesResponse));

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
