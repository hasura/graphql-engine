export const metadata = {
  resource_version: 45,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'bikes',
        kind: 'mssql',
        tables: [
          {
            table: {
              name: 'customers',
              schema: 'sales',
            },
          },
        ],
        configuration: {
          connection_info: {
            connection_string:
              'DRIVER={ODBC Driver 17 for SQL Server};SERVER=host.docker.internal;DATABASE=bikes;Uid=SA;Pwd=reallyStrongPwd123',
            pool_settings: {
              idle_timeout: 5,
              max_connections: 50,
            },
          },
        },
      },
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'Album',
              schema: 'public',
            },
            object_relationships: [
              {
                name: 'local_table_object_relationship',
                using: {
                  foreign_key_constraint_on: 'ArtistId',
                },
              },
              {
                name: 'manual_object_relationship',
                using: {
                  manual_configuration: {
                    column_mapping: {
                      AlbumId: 'ArtistId',
                    },
                    insertion_order: null,
                    remote_table: {
                      name: 'Artist',
                      schema: 'public',
                    },
                  },
                },
              },
            ],
            array_relationships: [
              {
                name: 'array_manual_relationship',
                using: {
                  manual_configuration: {
                    column_mapping: {
                      AlbumId: 'AlbumId',
                    },
                    insertion_order: null,
                    remote_table: {
                      name: 'Track',
                      schema: 'public',
                    },
                  },
                },
              },
              {
                name: 'local_table_array_relationship',
                using: {
                  foreign_key_constraint_on: {
                    column: 'AlbumId',
                    table: {
                      name: 'Track',
                      schema: 'public',
                    },
                  },
                },
              },
            ],
            remote_relationships: [
              {
                definition: {
                  hasura_fields: ['AlbumId'],
                  remote_field: {
                    country: {
                      arguments: {
                        code: '$AlbumId',
                      },
                    },
                  },
                  remote_schema: 'trevorblades',
                },
                name: 'legacy_remote_schema_relationship',
              },
              {
                definition: {
                  to_source: {
                    field_mapping: {
                      AlbumId: 'customer_id',
                    },
                    relationship_type: 'array',
                    source: 'bikes',
                    table: {
                      name: 'customers',
                      schema: 'sales',
                    },
                  },
                },
                name: 'remote_db_array_relationship',
              },
              {
                definition: {
                  to_source: {
                    field_mapping: {
                      AlbumId: 'customer_id',
                    },
                    relationship_type: 'object',
                    source: 'bikes',
                    table: {
                      name: 'customers',
                      schema: 'sales',
                    },
                  },
                },
                name: 'remote_db_object_relationship',
              },
              {
                definition: {
                  to_remote_schema: {
                    lhs_fields: ['AlbumId'],
                    remote_field: {
                      country: {
                        arguments: {
                          code: '$AlbumId',
                        },
                      },
                    },
                    remote_schema: 'trevorblades',
                  },
                },
                name: 'source_to_remote_schema_relationship',
              },
            ],
          },
          {
            table: {
              name: 'Artist',
              schema: 'public',
            },
          },
          {
            table: {
              name: 'Track',
              schema: 'public',
            },
          },
        ],
        configuration: {
          connection_info: {
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
      },
    ],
    remote_schemas: [
      {
        name: 'trevorblades',
        definition: {
          url: 'https://countries.trevorblades.com/',
          timeout_seconds: 60,
          customization: {},
        },
      },
    ],
  },
};

export const relationshipQueryResponse = {
  result_type: 'TuplesOk',
  result: [
    ['source_table', 'source_column', 'target_table', 'target_column'],
    ['"Album"', '"ArtistId"', '"Artist"', '"ArtistId"'],
    ['"Track"', '"AlbumId"', '"Album"', '"AlbumId"'],
    ['"Artist"', '"ArtistId"', '"Album"', '"Name"'],
  ],
};
