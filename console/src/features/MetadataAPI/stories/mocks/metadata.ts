export const metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: 'public',
              name: 'user',
            },
            array_relationships: [
              {
                name: 'products',
                using: {
                  foreign_key_constraint_on: {
                    column: 'fk_user_id',
                    table: {
                      schema: 'public',
                      name: 'product',
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              schema: 'public',
              name: 'product',
            },
            object_relationships: [
              {
                name: 'user',
                using: {
                  foreign_key_constraint_on: 'fk_user_id',
                },
              },
            ],
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: true,
            database_url: {
              from_env: 'HASURA_GRAPHQL_DATABASE_URL',
            },
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
    ],
  },
};
