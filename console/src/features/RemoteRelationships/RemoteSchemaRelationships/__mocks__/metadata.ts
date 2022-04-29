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
              name: 'resident',
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
        name: 'chinook',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: 'public',
              name: 'Album',
            },
            object_relationships: [
              {
                name: 'relt1obj',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'user',
                      name: 'userAddress',
                    },
                    insertion_order: null,
                    column_mapping: {
                      AlbumId: 'Id',
                      Title: 'Country',
                    },
                  },
                },
              },
            ],
            array_relationships: [
              {
                name: 'relt1array',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'public',
                      name: 'Artist',
                    },
                    insertion_order: null,
                    column_mapping: {
                      AlbumId: 'Id',
                      Title: 'Name',
                    },
                  },
                },
              },
            ],
            remote_relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'default',
                    table: {
                      schema: 'public',
                      name: 'resident',
                    },
                    field_mapping: {
                      id: 'Id',
                      updated_at: 'FirstName',
                    },
                  },
                },
                name: 'AlbumToResident',
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
          {
            table: {
              schema: 'public',
              name: 'comedies',
            },
          },
          {
            table: {
              schema: 'user',
              name: 'userAddress',
            },
          },
          {
            table: {
              schema: 'user',
              name: 'userInfo',
            },
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'read-committed',
          },
        },
      },
    ],
    remote_schemas: [
      {
        name: 'source_remote_schema',
        definition: {
          url: 'https://countries.trevorblades.com/',
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
      {
        name: 'with_default_values',
        definition: {
          url: 'https://some-graph-other-endpoint.com/api/graphql',
          timeout_seconds: 60,
          forward_client_headers: true,
        },
        comment: '',
        remote_relationships: [
          {
            relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'default',
                    table: {
                      schema: 'public',
                      name: 'resident',
                    },
                    field_mapping: {
                      name: 'Title',
                    },
                  },
                },
                name: 'testRemoteRelationship',
              },
              {
                definition: {
                  to_remote_schema: {
                    remote_field: {
                      country: {
                        arguments: {
                          code: '$code',
                        },
                      },
                    },
                    remote_schema: 'source_remote_schema',
                    lhs_fields: ['code'],
                  },
                },
                name: 'an_example_rs_to_rs_relationship',
              },
            ],
            type_name: 'Country',
          },
        ],
      },
    ],
  },
};
