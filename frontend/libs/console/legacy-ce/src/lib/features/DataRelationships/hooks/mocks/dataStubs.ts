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
          },
          {
            table: {
              schema: 'public',
              name: 'product',
            },
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

export const metadataWithExistingRelationships = {
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
              name: 'product',
            },
            object_relationships: [
              {
                name: 'user',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'public',
                      name: 'user',
                    },
                    insertion_order: null,
                    column_mapping: {
                      fk_user_id: 'id',
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              schema: 'public',
              name: 'user',
            },
            array_relationships: [
              {
                name: 'products',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'public',
                      name: 'product',
                    },
                    insertion_order: null,
                    column_mapping: {
                      id: 'fk_user_id',
                    },
                  },
                },
              },
            ],
          },
        ],
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
  },
};

export const query = {
  result_type: 'TuplesOk',
  result: [
    ['source_table', 'source_column', 'target_table', 'target_column'],
    ['product', 'fk_user_id', '"user"', 'id'],
  ],
};
