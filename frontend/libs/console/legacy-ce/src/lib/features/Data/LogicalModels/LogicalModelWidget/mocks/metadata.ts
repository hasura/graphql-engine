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
      {
        name: 'bikes',
        kind: 'mssql',
        tables: [],
        stored_procedures: [
          {
            arguments: {
              test: {
                nullable: true,
                type: 'varchar',
              },
            },
            configuration: {
              exposed_as: 'query',
            },
            returns: 'logical_model_1',
            stored_procedure: {
              name: 'stored_procedure_1',
              schema: 'dbo',
            },
          },
          {
            arguments: {
              name: {
                nullable: true,
                type: 'varchar',
              },
            },
            configuration: {
              exposed_as: 'query',
            },
            returns: 'logical_model_2',
            stored_procedure: {
              name: 'stored_procedure_2',
              schema: 'dbo',
            },
          },
        ],
        logical_models: [
          {
            fields: [
              {
                name: 'TABLE_QUALIFIER',
                nullable: false,
                type: 'sysname',
              },
              {
                name: 'TABLE_OWNER',
                nullable: false,
                type: 'sysname',
              },
              {
                name: 'TABLE_NAME',
                nullable: false,
                type: 'sysname',
              },
              {
                name: 'TABLE_TYPE',
                nullable: false,
                type: 'varchar',
              },
              {
                name: 'REMARKS',
                nullable: false,
                type: 'varchar',
              },
            ],
            name: 'logical_model_1',
          },
          {
            fields: [
              {
                name: 'TABLE_QUALIFIER',
                nullable: false,
                type: 'sysname',
              },
              {
                name: 'TABLE_OWNER',
                nullable: false,
                type: 'sysname',
              },
              {
                name: 'TABLE_NAME',
                nullable: false,
                type: 'sysname',
              },
              {
                name: 'TABLE_TYPE',
                nullable: false,
                type: 'varchar',
              },
              {
                name: 'REMARKS',
                nullable: false,
                type: 'varchar',
              },
            ],
            name: 'logical_model_2',
          },
        ],
        configuration: {
          connection_info: {
            connection_string:
              'DRIVER={ODBC Driver 17 for SQL Server};SERVER=host.docker.internal;DATABASE=bikes;Uid=SA;Pwd=reallyStrongPwd123',
            pool_settings: {
              idle_timeout: 5,
              max_connections: null,
              total_max_connections: null,
            },
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
