export const metadata = {
  resource_version: 1,
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
    remote_schemas: [
      {
        name: 'remoteSchema1',
        definition: {
          url: 'https://some-graph-endpoint.com/api/graphql',
          timeout_seconds: 60,
          forward_client_headers: true,
        },
        comment: '',
      },
      {
        name: 'remoteSchema2',
        definition: {
          url: 'https://some-graph-other-endpoint.com/api/graphql',
          timeout_seconds: 60,
          forward_client_headers: true,
        },
        comment: '',
      },
    ],
  },
};
