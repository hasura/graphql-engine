export const metadata = {
  resource_version: 75,
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
              name: 'Album',
            },
            object_relationships: [
              {
                name: 'newmanual',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'public',
                      name: 'Album',
                    },
                    insertion_order: null,
                    column_mapping: {
                      Title: 'ArtistId',
                    },
                  },
                },
              },
              {
                name: 'testName',
                using: {
                  foreign_key_constraint_on: 'ArtistId',
                },
              },
              {
                name: 'testNewRnNamerr',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'public',
                      name: 'Album',
                    },
                    insertion_order: null,
                    column_mapping: {
                      AlbumId: 'ArtistId',
                    },
                  },
                },
              },
            ],
            array_relationships: [
              {
                name: 'TracksName',
                using: {
                  foreign_key_constraint_on: {
                    column: 'AlbumId',
                    table: {
                      schema: 'public',
                      name: 'Track',
                    },
                  },
                },
              },
              {
                name: 'manualReln',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'public',
                      name: 'Artist',
                    },
                    insertion_order: null,
                    column_mapping: {
                      Title: 'Name',
                    },
                  },
                },
              },
            ],
            remote_relationships: [
              {
                definition: {
                  remote_field: {
                    country: {
                      arguments: {
                        code: '$AlbumId',
                      },
                    },
                  },
                  hasura_fields: ['AlbumId'],
                  remote_schema: 'countries',
                },
                name: 'RS_countries',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'test2',
                    table: {
                      schema: '_onetoone',
                      name: 'owner',
                    },
                    field_mapping: {
                      AlbumId: 'id',
                    },
                  },
                },
                name: 'test',
              },
            ],
          },
          {
            table: {
              schema: 'public',
              name: 'Artist',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'Customer',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'Employee',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'Genre',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'Invoice',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'InvoiceLine',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'MediaType',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'Playlist',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'PlaylistTrack',
            },
          },
          {
            table: {
              schema: 'public',
              name: 'Track',
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
      {
        name: 'test2',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: '_onetoone',
              name: 'owner',
            },
          },
          {
            table: {
              schema: '_onetoone',
              name: 'passport_info',
            },
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url: {
              from_env: 'HASURA_GRAPHQL_DATABASE_URL',
            },
            isolation_level: 'read-committed',
          },
        },
      },
    ],
    remote_schemas: [
      {
        name: 'c',
        definition: {
          url: 'https://countries.trevorblades.com/',
          timeout_seconds: 60,
          customization: {
            type_names: {
              suffix: 'c',
              prefix: 'c',
              mapping: {},
            },
            root_fields_namespace: 'cc',
          },
        },
      },
      {
        name: 'countries',
        definition: {
          url: 'https://countries.trevorblades.com/',
          timeout_seconds: 60,
        },
        comment: '',
        remote_relationships: [
          {
            relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'array',
                    source: 'default',
                    table: {
                      schema: 'public',
                      name: 'Album',
                    },
                    field_mapping: {
                      code: 'AlbumId',
                    },
                  },
                },
                name: 'test',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'array',
                    source: 'default',
                    table: {
                      schema: 'public',
                      name: 'Album',
                    },
                    field_mapping: {
                      name: 'Title',
                    },
                  },
                },
                name: 'sss',
              },
            ],
            type_name: 'Country',
          },
        ],
      },
    ],
    custom_types: {
      objects: [
        {
          name: 'LoginResponse',
          fields: [
            {
              name: 'accessToken',
              type: 'String!',
            },
          ],
        },
        {
          name: 'AddResult',
          fields: [
            {
              name: 'sum',
              type: 'Int',
            },
          ],
        },
      ],
    },
  },
};

export const relationshipQueryResponse = {
  result_type: 'TuplesOk',
  result: [
    ['source_table', 'source_column', 'target_table', 'target_column'],
    ['"Album"', '"ArtistId"', '"Artist"', '"ArtistId"'],
    ['"Track"', '"AlbumId"', '"Album"', '"AlbumId"'],
  ],
};
