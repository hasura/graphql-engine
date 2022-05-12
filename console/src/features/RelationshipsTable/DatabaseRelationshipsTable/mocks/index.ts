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
              name: 'user',
            },
            remote_relationships: [
              {
                definition: {
                  to_remote_schema: {
                    remote_field: {
                      test: {
                        arguments: {
                          where: {
                            id: {
                              _eq: '$id',
                            },
                          },
                        },
                      },
                    },
                    remote_schema: 'remoteSchema1',
                    lhs_fields: ['id'],
                  },
                },
                name: 'new_payload',
              },
              {
                definition: {
                  remote_field: {
                    test: {
                      arguments: {
                        where: {
                          id: {
                            _eq: '$id',
                          },
                        },
                      },
                    },
                  },
                  hasura_fields: ['id'],
                  remote_schema: 'remoteSchema1',
                },
                name: 'legacy_payload',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bigquery_test',
                    table: {
                      dataset: 'sensei',
                      name: 'table1',
                    },
                    field_mapping: {
                      id: 'field1',
                    },
                  },
                },
                name: 'object_relationship',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'array',
                    source: 'bigquery_test',
                    table: {
                      dataset: 'sensei',
                      name: 'table1',
                    },
                    field_mapping: {
                      name: 'id',
                    },
                  },
                },
                name: 'array_relationship',
              },
            ],
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
    ],
  },
};
