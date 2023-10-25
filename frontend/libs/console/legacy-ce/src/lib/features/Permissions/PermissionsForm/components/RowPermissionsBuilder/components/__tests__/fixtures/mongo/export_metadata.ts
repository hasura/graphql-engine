export default {
  resource_version: 17,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'M',
        kind: 'Mongo',
        tables: [
          {
            table: ['students'],
            select_permissions: [
              {
                role: 'v',
                permission: {
                  columns: [],
                  filter: {
                    address: {
                      city: {
                        _eq: 'Moon',
                      },
                    },
                  },
                },
                comment: '',
              },
            ],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            connection: 'mongodb://localhost:27017',
            db: 'sample',
          },
        },
      },
      {
        name: 'Postgres',
        kind: 'postgres',
        tables: [],
        configuration: {
          connection_info: {
            database_url: 'postgres://postgres:pass@postgres:5432/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        Mongo: {
          uri: 'http://host.docker.internal:3000',
        },
      },
    },
  },
};
