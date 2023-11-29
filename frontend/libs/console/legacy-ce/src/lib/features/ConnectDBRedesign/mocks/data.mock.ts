import { Metadata } from '../../hasura-metadata-types';

export const mockMetadata: Metadata = {
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
              connection_lifetime: 100,
              idle_timeout: 200,
              pool_timeout: 300,
              retries: 400,
              total_max_connections: 500,
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
            connection_string:
              'DRIVER={ODBC Driver 17 for SQL Server};SERVER=host.docker.internal;DATABASE=bikes;Uid=SA;Pwd=reallyStrongPwd123',
            pool_settings: {
              total_max_connections: 50,
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
      {
        name: 'bigquery_test',
        kind: 'bigquery',
        tables: [],
        configuration: {
          datasets: ['sample_dataset', 'sample_dataset_2'],
          global_select_limit: '1.0',
          project_id: 'hasura-test',
          service_account: {
            client_email: 'service-account@someemail.com',
            private_key:
              '-----BEGIN PRIVATE KEY-----\nsecretkey\n-----END PRIVATE KEY-----\n',
            project_id: 'hasura-test',
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

export const mockSourceKinds = {
  agentsAdded: {
    sources: [
      {
        available: true,
        builtin: true,
        display_name: 'pg',
        kind: 'pg',
      },
      {
        available: true,
        builtin: true,
        display_name: 'citus',
        kind: 'citus',
      },
      {
        available: true,
        builtin: true,
        display_name: 'cockroach',
        kind: 'cockroach',
      },
      {
        available: true,
        builtin: true,
        display_name: 'mssql',
        kind: 'mssql',
      },
      {
        available: true,
        builtin: true,
        display_name: 'bigquery',
        kind: 'bigquery',
      },
      {
        available: true,
        builtin: false,
        display_name: 'Hasura SQLite',
        kind: 'sqlite',
      },
      {
        available: true,
        builtin: false,
        display_name: 'Amazon Athena',
        kind: 'athena',
        release_name: 'Beta',
      },
      {
        available: true,
        builtin: false,
        display_name: 'Snowflake',
        kind: 'snowflake',
        release_name: 'Beta',
      },
      {
        available: true,
        builtin: false,
        display_name: 'MySQL',
        kind: 'mysql8',
        release_name: 'Alpha',
      },
    ],
  },
  agentsAddedSuperConnectorNotAvailable: {
    sources: [
      {
        available: true,
        builtin: true,
        display_name: 'pg',
        kind: 'pg',
      },
      {
        available: true,
        builtin: true,
        display_name: 'citus',
        kind: 'citus',
      },
      {
        available: true,
        builtin: true,
        display_name: 'cockroach',
        kind: 'cockroach',
      },
      {
        available: true,
        builtin: true,
        display_name: 'mssql',
        kind: 'mssql',
      },
      {
        available: true,
        builtin: true,
        display_name: 'bigquery',
        kind: 'bigquery',
      },
      {
        available: false,
        builtin: false,
        //display_name: 'Hasura SQLite',
        kind: 'sqlite',
      },
      {
        available: false,
        builtin: false,
        //display_name: 'Amazon Athena',
        kind: 'athena',
        release_name: 'Beta',
      },
      {
        available: false,
        builtin: false,
        //display_name: 'Snowflake',
        kind: 'snowflake',
        release_name: 'Beta',
      },
      {
        available: false,
        builtin: false,
        //display_name: 'MySQL',
        kind: 'mysql8',
        release_name: 'Alpha',
      },
    ],
  },
  agentsNotAdded: {
    sources: [
      {
        available: true,
        builtin: true,
        display_name: 'pg',
        kind: 'pg',
      },
      {
        available: true,
        builtin: true,
        display_name: 'citus',
        kind: 'citus',
      },
      {
        available: true,
        builtin: true,
        display_name: 'cockroach',
        kind: 'cockroach',
      },
      {
        available: true,
        builtin: true,
        display_name: 'mssql',
        kind: 'mssql',
      },
      {
        available: true,
        builtin: true,
        display_name: 'bigquery',
        kind: 'bigquery',
      },
      {
        available: true,
        builtin: false,
        display_name: 'Hasura SQLite',
        kind: 'sqlite',
      },
    ],
  },
};
