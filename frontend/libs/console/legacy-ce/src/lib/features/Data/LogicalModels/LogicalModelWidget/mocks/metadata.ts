import { Metadata } from '../../../../hasura-metadata-types';

// make sure we are using the Metadata type and we won't create phantom errors in the code because our objects don't adhere to the underlying types!
export const metadata: Metadata = {
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
                type: { scalar: 'text', nullable: true },
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
                type: { scalar: 'sysname', nullable: false },
              },
              {
                name: 'TABLE_OWNER',

                type: { scalar: 'sysname', nullable: false },
              },
              {
                name: 'TABLE_NAME',
                type: { scalar: 'sysname', nullable: false },
              },
              {
                name: 'TABLE_TYPE',
                type: { scalar: 'varchar', nullable: false },
              },
              {
                name: 'REMARKS',
                type: { scalar: 'varchar', nullable: false },
              },
            ],
            name: 'logical_model_1',
          },
          {
            fields: [
              {
                name: 'TABLE_QUALIFIER',
                type: { scalar: 'sysname', nullable: false },
              },
              {
                name: 'TABLE_OWNER',
                type: { scalar: 'sysname', nullable: false },
              },
              {
                name: 'TABLE_NAME',
                type: { scalar: 'sysname', nullable: false },
              },
              {
                name: 'TABLE_TYPE',
                type: { scalar: 'varchar', nullable: false },
              },
              {
                name: 'REMARKS',
                type: { scalar: 'varchar', nullable: false },
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
              total_max_connections: null,
            },
          },
        },
      },
      {
        name: 'mssql',
        kind: 'mssql',
        tables: [
          {
            table: {
              name: 'Album',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'Artist',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'Customer',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'Employee',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'Genre',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'Invoice',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'InvoiceLine',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'MediaType',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'Playlist',
              schema: 'dbo',
            },
          },
          {
            table: {
              name: 'PlaylistTrack',
              schema: 'dbo',
            },
          },
        ],
        logical_models: [
          {
            fields: [
              {
                name: 'TABLE_QUALIFIER',
                type: {
                  nullable: true,
                  scalar: 'sysname',
                },
              },
              {
                name: 'TABLE_OWNER',
                type: {
                  nullable: true,
                  scalar: 'sysname',
                },
              },
              {
                name: 'TABLE_NAME',
                type: {
                  nullable: true,
                  scalar: 'sysname',
                },
              },
              {
                name: 'TABLE_TYPE',
                type: {
                  nullable: true,
                  scalar: 'varchar',
                },
              },
              {
                name: 'REMARKS',
                type: {
                  nullable: true,
                  scalar: 'varchar',
                },
              },
            ],
            name: 'tables',
          },
        ],
        configuration: {
          connection_info: {
            connection_string:
              'Driver={ODBC Driver 18 for SQL Server};Server=mssql,1433;Database=Chinook;UID=sa;PWD=Password!;Encrypt=yes;TrustServerCertificate=yes;ConnectionTimeout=30;',
            pool_settings: {
              idle_timeout: 5,
              total_max_connections: null,
            },
          },
        },
        customization: {
          naming_convention: 'hasura-default',
          type_names: {
            prefix: 'mssql_',
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
