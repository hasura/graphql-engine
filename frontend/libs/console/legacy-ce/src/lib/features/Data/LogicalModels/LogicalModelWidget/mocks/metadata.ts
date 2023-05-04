export const metadata = {
  resource_version: 5,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'Album',
              schema: 'public',
            },
          },
        ],
        logical_models: [
          {
            fields: [
              {
                name: 'a',
                nullable: false,
                type: 'text',
              },
            ],
            name: 'a',
          },
          {
            fields: [],
            name: 'as',
          },
        ],
        configuration: {
          connection_info: {
            database_url: 'postgres://postgres:pass@postgres:5432/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
        customization: {
          root_fields: {
            namespace: 'postgres',
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        mysqlgdc: {
          uri: 'http://data-connector-agent:8081/api/v1/mysql',
        },
        sqlite: {
          uri: 'http://sqlite:8100',
        },
      },
    },
  },
};
