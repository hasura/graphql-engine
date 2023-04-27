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
              '-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCodYF/0N3pmv9N\nUzpP3VSUTE7hIcN9a2Y82BpQ1qniX1AA2XpbKDpg5ItV9Ay5sTI5iCThxzQ6G8zo\nc340sJB9wVbSDRatgYxmazE4ie8atyovGGUkYkSixXJ9FCAZMT5IJdltLVPzX2dn\nAlsIgKqeWZ7La7v+1mgbsrf8Q7cN1rJ5gUWzjh4tUusxzkDN7VUVsopADyEIrzrs\nyPeqs5w3yUIRXlDQC3jnoEkZw9V+xI8+tg+y1neP45IJ+eGwFa/VCMh7dd24azdL\ngBd+y/vTmSa3912c69hvbPiLT2YGE6slngLei3Y8tBxMlIFbGYs9KduEbs6AwTWC\naNUqdbyXAgMBAAECggEAAJ47aoyAH0OUEjWbowEinm7nnuxH5SoqtOmrxZ0Vrng+\nZcMnuTfOe8iMGNwjq3lbDWCXay8b3Qh97dGos7Ul86E6P5QEIY8NOwZn6BUPI+t0\nJZgvMoRiRvkRTTyBkFLHX+Icf8Asm++RVB2IRsl5ZBJizqEUGqEbH2Gv1sXvb4vP\n0BZ4sgkjzLV6ZDoSUSjBJ0EUX0AAS6oj0HlxIzAlHTwJa9wulg2gzRNaODTx07aX\nLhGavJihkxgq6XjJZg8WHoJSjW9bKSR19m/k0pUhKKrZKmVqu73rpoCkL54HSKLH\nIdO/hJWpEdi7CNt00b8JAdpek2/ZfMHJc1ehPWgRQQKBgQDUJ1vhfqI1p5KoEMPZ\nraKzo4ZK5HdQCexjhmmgp3V+F0B3+45V23emfPnVVIt2nYT6dDXq0d2YqsjllFEO\nKxxxofZ31zczryLZzJaxVXteB8dT6QT5hnicKgsocu1JFoxsH8EbjzCf/wmBaNJr\nf125lnpinY7T3uFpHtgih0C1VwKBgQDLRlfwvQvcgr5bhxJ7OoyvjtSIhpLi/1yz\naaesy/vR38sm5Z0qvkZcSdhiyLDylTx1/RQSWIKyfkqqO20lDnxVI0PN6W6Cne2r\nrEfSv0vE/cx6Me20DlypZ+R4NcNo6TIQEWjvWicRnvMs9bxX49Kt7LrY0CARNQ/L\n6O2iN8FqwQKBgBoVxMxS/6rNMdEMTqhjGxAvWQG4WZszvSb3jeE89ctNHJYkDfos\ng+eqkiVSwdsRvxn0U/TNXQu6Y/0mYVj2oAawvVc61QS70fMpnpWrRwHmvvFoQz5S\n9F1vJfQRPW+xIA7jUKEM2BO0Df0lYEnrXSDGicG6UadnThvYuQZ1ztT9AoGBAMC7\nb4IIHIj1RfuONA2209FYwVa5Eeg8au6JTLvfpGG3qOy2ZLA0PI4gThRLaMp4iRzt\n2ewTE8idSbniIngyJdyrVQyVOXyKtOBCts6xJewfH5L0aAaCX4UXxl3by+1avWbS\nuwXGGqjwZmzIic1ja0wQ2o/btava/aE+tqFJ8A+BAoGAUXY05JQJ3EXjlzYdMkAh\nfH6sw3hoMiwV/jvx3NoXjdA45z/odt3N8ksfuOqbKmdg6A8smUhpQd3wILYCMwKN\nrG0BhtgFsmDHxoiBRys8CfVCVPj5f9SQRZPexopNzN7q4ncesbtALINhT72CnsPp\nzw9aMU09pFVFA8YCc+iGpX0=\n-----END PRIVATE KEY-----\n',
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
