import type { MetadataResponse } from '../../../../../../src/features/MetadataAPI/types';

export const export_metadata: MetadataResponse = {
  resource_version: 693,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [],
        configuration: {
          connection_info: {
            use_prepared_statements: true,
            database_url: { from_env: 'HASURA_GRAPHQL_DATABASE_URL' },
            isolation_level: 'read-committed',
            pool_settings: {
              connection_lifetime: 600,
              retries: 1,
              idle_timeout: 180,
              max_connections: 50,
            },
          },
        },
      },
      {
        name: 'postgres_db_with_env_var',
        kind: 'postgres',
        tables: [],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url: { from_env: 'HASURA_GRAPHQL_DATABASE_URL' },
            isolation_level: 'read-committed',
          },
        },
        customization: { naming_convention: 'hasura-default' },
      },
      {
        name: 'postgres_db_with_url',
        kind: 'postgres',
        tables: [{ table: { schema: 'temp_1', name: 'temp_table' } }],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:postgrespassword@postgres:5432/postgres',
            isolation_level: 'read-committed',
            pool_settings: {
              connection_lifetime: 600,
              retries: 1,
              idle_timeout: 180,
              max_connections: 50,
            },
          },
        },
        customization: { naming_convention: 'hasura-default' },
      },
      {
        name: 'test',
        kind: 'postgres',
        tables: [],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:postgrespassword@postgres:5432/postgres',
            isolation_level: 'read-committed',
          },
        },
      },
    ],
    query_collections: [
      {
        name: 'allowed-queries',
        definition: {
          queries: [
            {
              name: 'temp_table',
              query:
                'query MyQuery {\n\n  temp_1_temp_table {\n    name\n  }\n}',
            },
          ],
        },
      },
    ],
    allowlist: [{ collection: 'allowed-queries', scope: { global: true } }],
    custom_types: {
      objects: [
        { name: 'AddResult', fields: [{ name: 'sum', type: 'Int' }] },
        {
          name: 'LoginResponse',
          fields: [{ name: 'accessToken', type: 'String!' }],
        },
      ],
    },
    rest_endpoints: [
      {
        definition: {
          query: {
            collection_name: 'allowed-queries',
            query_name: 'temp_table',
          },
        },
        url: 'temp_table',
        methods: ['GET'],
        name: 'temp_table',
        comment: '',
      },
    ],
  },
};
