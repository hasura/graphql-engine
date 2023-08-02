import { rest } from 'msw';
import { HasuraMetadataV3 } from '../../../metadata/types';

const baseUrl = 'http://localhost:8080';

export const schemaList = {
  result_type: 'TuplesOk',
  result: [['schema_name'], ['public'], ['default']],
};

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v1/metadata`, (_req, res, ctx) => {
    return res(
      ctx.json({
        resource_version: 1,
        metadata: {
          version: 3,
          sources: [
            {
              name: 'default',
              kind: 'postgres',
              tables: [{ table: { schema: 'public', name: 'table' } }],
              configuration: {
                connection_info: {
                  use_prepared_statements: true,
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
            },
            {
              name: 'default2',
              kind: 'postgres',
              tables: [{ table: { schema: 'public', name: 'table' } }],
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
              name: 'default3',
              kind: 'postgres',
              tables: [{ table: { schema: 'public', name: 'table' } }],
              configuration: {
                connection_info: {
                  use_prepared_statements: false,
                  database_url: {
                    connection_parameters: {
                      database: 'postgres',
                      username: 'postgres',
                      password: 'postgrespassword',
                      host: 'postgres',
                      port: 5432,
                    },
                  },
                  isolation_level: 'read-committed',
                },
              },
            },
          ],
        } as HasuraMetadataV3,
      })
    );
  }),
];
