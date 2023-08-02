export default {
  resource_version: 125,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'Author',
              schema: 'public',
            },
            select_permissions: [
              {
                role: 'editor',
                permission: {
                  columns: [],
                  filter: {
                    name: {
                      _ceq: ['$', 'name'],
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              name: 'Cart',
              schema: 'public',
            },
          },
          {
            table: {
              name: 'Record',
              schema: 'public',
            },
            object_relationships: [
              {
                name: 'Author',
                using: {
                  manual_configuration: {
                    column_mapping: {
                      author_id: 'id',
                    },
                    insertion_order: null,
                    remote_table: {
                      name: 'Author',
                      schema: 'public',
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              name: 'Stuff',
              schema: 'public',
            },
            object_relationships: [
              {
                name: 'Cart',
                using: {
                  foreign_key_constraint_on: 'cart_id',
                },
              },
            ],
            insert_permissions: [
              {
                role: 'editor',
                permission: {
                  check: {
                    name: {
                      _eq: 'Name',
                    },
                  },
                  columns: ['id', 'name', 'enabled', 'cart_id', 'jason', 'box'],
                },
              },
            ],
            select_permissions: [
              {
                role: 'editor',
                permission: {
                  columns: ['id', 'name', 'enabled', 'cart_id', 'jason', 'box'],
                  filter: {
                    jason: {
                      _clt: ['jason'],
                    },
                  },
                },
              },
            ],
            update_permissions: [
              {
                role: 'editor',
                permission: {
                  columns: ['id', 'name', 'enabled', 'cart_id', 'jason', 'box'],
                  filter: {
                    box: {
                      _eq: 'X-Hasura-User-Id',
                    },
                  },
                  check: {
                    _and: [
                      {
                        box: {
                          _eq: 'X-Hasura-User-Id',
                        },
                      },
                    ],
                  },
                  set: {
                    enabled: 'true',
                    id: 1,
                  },
                },
              },
            ],
            delete_permissions: [
              {
                role: 'editor',
                permission: {
                  filter: {
                    name: {
                      _eq: 'Name',
                    },
                  },
                },
              },
            ],
          },
        ],
        configuration: {
          connection_info: {
            database_url: {
              from_env: 'HASURA_GRAPHQL_DATABASE_URL',
            },
            isolation_level: 'read-committed',
            pool_settings: {
              connection_lifetime: 600,
              idle_timeout: 180,
              max_connections: 50,
              retries: 1,
            },
            use_prepared_statements: true,
          },
        },
      },
      {
        name: 'Big',
        kind: 'bigquery',
        tables: [
          {
            table: {
              dataset: 'samples_for_documentation',
              name: 'articles',
            },
            insert_permissions: [
              {
                role: 'editor',
                permission: {
                  check: {
                    id: {
                      _eq: -1,
                    },
                  },
                  columns: ['id', 'author_id', 'title', 'body', 'published_on'],
                },
              },
            ],
            select_permissions: [
              {
                role: 'editor',
                permission: {
                  columns: ['id', 'author_id', 'title', 'body', 'published_on'],
                  filter: {},
                },
              },
            ],
          },
          {
            table: {
              dataset: 'samples_for_documentation',
              name: 'authors',
            },
          },
        ],
        configuration: {
          datasets: ['horses', 'luca', 'samples_for_documentation'],
          global_select_limit: '1000.0',
          project_id: 'regency-polecat-beehive',
          service_account: {
            client_email:
              'bigquery-testing@regency-polecat-beehive.iam.gserviceaccount.com',
            private_key:
              '-----BEGIN PRIVATE KEY-----\nsecret\n-----END PRIVATE KEY-----\n',
            project_id: 'regency-polecat-beehive',
          },
        },
      },
      {
        name: 'Chinook',
        kind: 'mysql8',
        tables: [
          {
            table: ['Chinook', 'Customer'],
            insert_permissions: [
              {
                role: 'editor',
                permission: {
                  check: {},
                  columns: [],
                },
              },
            ],
            select_permissions: [
              {
                role: 'editor',
                permission: {
                  columns: [
                    'CustomerId',
                    'FirstName',
                    'LastName',
                    'Company',
                    'Address',
                    'City',
                    'State',
                    'Country',
                    'PostalCode',
                    'Phone',
                    'Fax',
                    'Email',
                    'SupportRepId',
                  ],
                  filter: {
                    Phone: {
                      _eq: 'X-Hasura-User-I',
                    },
                  },
                },
              },
            ],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            jdbc_url: 'jdbc:mysql://mysql:3306/Chinook?user=root&password=pass',
          },
        },
      },
      {
        name: 'MySQL',
        kind: 'mysql8',
        tables: [],
        configuration: {
          template: null,
          timeout: null,
          value: {
            jdbc_url: 'jdbc:mysql://mysql:3306/mysql?user=root&password=pass',
          },
        },
      },
      {
        name: 'SQLite',
        kind: 'SQLite',
        tables: [
          {
            table: ['Album'],
            insert_permissions: [
              {
                role: 'editor',
                permission: {
                  check: {
                    AlbumId: {
                      _eq: 'X-Hasura-User-Id',
                    },
                  },
                  columns: [],
                },
              },
            ],
            select_permissions: [
              {
                role: 'editor',
                permission: {
                  columns: [],
                  filter: {
                    AlbumId: {
                      _eq: 'X-Hasura-User-Id',
                    },
                  },
                },
              },
            ],
          },
          {
            table: ['Track'],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            db: '/chinook.db',
            explicit_main_schema: false,
            include_sqlite_meta_tables: false,
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        MySQL2: {
          uri: 'http://mysql-data-connector-agent1:8081',
        },
        MySQLSQL: {
          uri: 'http://mysql-data-connector-agent1:8081',
        },
        NewMy: {
          uri: 'http://mysql-data-connector-agent1:8081',
        },
        OhMy: {
          uri: 'http://mysql-data-connector-agent1:8081',
        },
        SQLite: {
          uri: 'http://host.docker.internal:8100',
        },
        mysql8: {
          uri: 'http://data-connector-agent:8081/api/v1/mysql',
        },
      },
    },
  },
};
