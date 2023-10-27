export default {
  resource_version: 31,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'OhMy',
        kind: 'mysql8',
        tables: [
          {
            table: ['Chinook', 'Album'],
          },
          {
            table: ['Chinook', 'Artist'],
            remote_relationships: [
              {
                definition: {
                  to_source: {
                    field_mapping: {
                      ArtistId: 'ArtistId',
                    },
                    relationship_type: 'object',
                    source: 'Postgres',
                    table: {
                      name: 'Album',
                      schema: 'public',
                    },
                  },
                },
                name: 'Album_Artist',
              },
            ],
            select_permissions: [
              {
                role: 'viewer',
                permission: {
                  columns: [],
                  filter: {
                    Name: {
                      _eq: '',
                    },
                  },
                },
                comment: '',
              },
            ],
          },
          {
            table: ['Chinook', 'Customer'],
          },
          {
            table: ['Chinook', 'Employee'],
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
            table: ['Chinook', 'PlaylistTrack'],
          },
          {
            table: ['Chinook', 'Track'],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            fully_qualify_all_names: true,
            jdbc_url:
              'jdbc:mysql://mysql:3306/Chinook?allowMultiQueries=true&user=root&password=pass',
          },
        },
      },
      {
        name: 'Postgres',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'Album',
              schema: 'public',
            },
          },
          {
            table: {
              name: 'Customer',
              schema: 'public',
            },
          },
        ],
        configuration: {
          connection_info: {
            database_url: 'postgres://postgres:pass@postgres:5432/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
      },
      {
        name: 'Slow',
        kind: 'snowflake',
        tables: [
          {
            table: ['ALBUM'],
            select_permissions: [
              {
                role: 'viewer',
                permission: {
                  columns: ['TITLE', 'ALBUMID', 'ARTISTID'],
                  filter: {
                    TITLE: {
                      _eq: 'X-Hasura-User-Id',
                    },
                  },
                },
                comment: '',
              },
            ],
          },
          {
            table: ['INVOICESBYCOUNTRY'],
          },
        ],
        functions: [
          {
            function: ['SEARCH_ALBUMS_BY_TITLE'],
            configuration: {
              custom_name: 'SEARCH_ALBUMS',
              custom_root_fields: {},
              response: {
                table: ['ALBUM'],
                type: 'table',
              },
            },
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            fully_qualify_all_names: false,
            jdbc_url:
              'jdbc:snowflake://ak41499.us-east-2.aws.snowflakecomputing.com?user=gdc&password=8yvF5qcRNXA2NPFQ&warehouse=COMPUTE_WH&db=CHINOOK&role=PUBLIC&schema=PUBLIC&CLIENT_SESSION_KEEP_ALIVE=TRUE',
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        athena: {
          uri: 'http://host.docker.internal:8081/api/v1/athena',
        },
        mariadb: {
          uri: 'http://host.docker.internal:8081/api/v1/mariadb',
        },
        mysql8: {
          uri: 'http://host.docker.internal:8081/api/v1/mysql',
        },
        oracle: {
          uri: 'http://host.docker.internal:8081/api/v1/oracle',
        },
        snowflake: {
          uri: 'http://host.docker.internal:8081/api/v1/snowflake',
        },
      },
    },
  },
};
