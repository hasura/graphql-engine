import { rest } from 'msw';

const metadata = {
  resource_version: 528,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'mssql',
        kind: 'mssql',
        tables: [],
        configuration: {
          connection_info: {
            connection_string:
              'Driver={ODBC Driver 18 for SQL Server};Server=mssql,1433;Database=Chinook;UID=sa;PWD=Password!;Encrypt=yes;TrustServerCertificate=yes;ConnectionTimeout=30;',
            pool_settings: {
              idle_timeout: 5,
              max_connections: null,
              total_max_connections: null,
            },
          },
        },
      },
      {
        name: 'mysql',
        kind: 'mysqlgdc',
        tables: [
          {
            table: ['Chinook', 'Album'],
            array_relationships: [
              {
                name: 'Chinook_Album_Chinook_Tracks',
                using: {
                  foreign_key_constraint_on: {
                    column: 'AlbumId',
                    table: ['Chinook', 'Track'],
                  },
                },
              },
            ],
          },
          {
            table: ['Chinook', 'Customer'],
          },
          {
            table: ['Chinook', 'Genre'],
          },
          {
            table: ['Chinook', 'Invoice'],
          },
          {
            table: ['Chinook', 'InvoiceLine'],
          },
          {
            table: ['Chinook', 'MediaType'],
          },
          {
            table: ['Chinook', 'Playlist'],
          },
          {
            table: ['Chinook', 'Track'],
            object_relationships: [
              {
                name: 'Chinook_Track_Chinook_Album',
                using: {
                  foreign_key_constraint_on: 'AlbumId',
                },
              },
            ],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            jdbc_url:
              'jdbc:mysql://mysql:3306/Chinook?allowMultiQueries=true&user=root&password=pass',
          },
        },
        customization: {
          root_fields: {
            namespace: 'mysql',
          },
          type_names: {},
        },
      },
      {
        name: 'postgres',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'directory',
              schema: '_fuzzysearch',
            },
          },
        ],
        native_queries: [
          {
            arguments: {},
            code: "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")\n",
            returns: 'hello_world',
            root_field_name: 'hello_world_function',
          },
          {
            arguments: {},
            code: "SELECT * FROM (VALUES ('hello', 'world2'), ('welcome', 'friend')) as t(\"one\", \"two\")\n",
            returns: 'hello_world2',
            root_field_name: 'hello_world_function2',
          },
        ],
        logical_models: [
          {
            fields: [
              {
                name: 'one',
                nullable: false,
                type: 'text',
              },
              {
                name: 'two',
                nullable: false,
                type: 'text',
              },
            ],
            name: 'hello_world',
          },
          {
            fields: [
              {
                name: 'one',
                nullable: false,
                type: 'text',
              },
              {
                name: 'two',
                nullable: false,
                type: 'text',
              },
            ],
            name: 'hello_world2',
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
        name: 'sqlite',
        kind: 'sqlite',
        tables: [],
        configuration: {
          template: null,
          timeout: null,
          value: {
            db: '/sqlite.db',
            explicit_main_schema: false,
            include_sqlite_meta_tables: false,
          },
        },
        customization: {
          root_fields: {
            namespace: 'sqlite',
          },
          type_names: {},
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

export const handlers = () => [
  rest.post('http://localhost:8080/v1/metadata', (req, res, ctx) => {
    const requestBody = req.body as Record<string, any>;

    if (requestBody.type === 'export_metadata') {
      ctx.delay();

      return res(ctx.json(metadata));
    }

    return res(ctx.json({}));
  }),
];
