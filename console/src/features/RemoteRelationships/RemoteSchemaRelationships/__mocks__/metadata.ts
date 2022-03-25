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
        name: 'bikes',
        kind: 'mssql',
        tables: [
          {
            table: {
              schema: 'production',
              name: 'brands',
            },
          },
          {
            table: {
              schema: 'production',
              name: 'categories',
            },
          },
          {
            table: {
              schema: 'sales',
              name: 'customers',
            },
          },
          {
            table: {
              schema: 'sales',
              name: 'order_items',
            },
          },
          {
            table: {
              schema: 'sales',
              name: 'orders',
            },
          },
          {
            table: {
              schema: 'production',
              name: 'products',
            },
          },
          {
            table: {
              schema: 'sales',
              name: 'staffs',
            },
          },
          {
            table: {
              schema: 'production',
              name: 'stocks',
            },
          },
          {
            table: {
              schema: 'sales',
              name: 'stores',
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
              schema: 'public',
              name: 'Album',
            },
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
      {
        name: 'test',
        kind: 'bigquery',
        tables: [
          {
            table: {
              dataset: 'dataset1',
              name: 'bq_test_table',
            },
          },
        ],
        configuration: {
          service_account: {
            project_id: 'hackanoodle',
            client_email:
              'starting-account-4jphfe9gkl8a@hackanoodle.iam.gserviceaccount.com',
            private_key:
              '-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC04A3VHqFCuy2R\n8v4q/Y9btponxnqD4HLOLcnvXjXeE7jku1TSKdVyTncZ8Ovv8eODtbeN8xoKPjmN\nWSmhllwefpzKH1o4s1euTh43DKZZtLLrlhiPvVazPr1FtdAAKh9HpoxfxQ+h4tg3\nBprMDi4oQ61qIjnwpRJXWKepaHtz4KPiU1cBiokB7Q6fw88BwMtoWKmznehu7rvV\nKN1LbDHdn4v5ALMKpvcL4gZ+z8XFe5jGaFvjqqJmkW6R+tRyr2jICNZU6Sku5xh3\nc8UdYSlJgAKBBR6Xrk+/L9fQzMhrunlvu/nJlA22lVI3dai+24Sq5v/e/YOKEds8\nx0VfrEGxAgMBAAECggEADKJreO/6K/0C8INYo4RheZKyZzWfLHvWzSXn6YlrShtW\n0pxuzvzm2OSX/3cDphe/kJ1WiGr7Lky9LK1WIgBMqEEoHtrMjLIjXWy9wdLnglPj\n3R63kb/UMPJgmoBaGo7Wr0S4IL71SfW8bHCQytDxQrUChavFIBUJyFHsKUJgAD6q\nGpJe2fOnyUZfvZC8UQ6N/aNqrgQVsOPxmSBuayAgYo6C0fFccaqpGQ+dsxRNKXvH\nrC8FFQL53jPW9JXSZt/oV87nemDul7YzvEmKec1TA0NA6gziA8MsZJncgvaFFTU7\nExlpoRqitfUo62K9rq08ZOhEn3rrgpB8CSij2JiDGQKBgQD38plyQeYSAcXaAQg+\nLqJbXP8wMMKfdZTK2zNL7VqGdM9lxCreLyOISKjdgd4lj56LCjGcXbXcrLzE/k80\nNUtF5Ixo5XRSzh7SUMTHS6eTke6eUP7JG7GbM/vNEuGgpdc9JtnVZfiMDAgqp5O6\n85oNxQ4lLWCJE/WnjYDgrQj7SQKBgQC6v9L6fatJs42eBVQOJ9NeLHlWwor/qa1Z\nLK2WeycJsHCdZ6aFgsSH8x1hdR5LFuhKxKd4ZZTf1MJVGSG2DVLYL9bretPQaE0c\nVGXKO9izigTts6AiZ/3mHAglAbUMkZAXyOFuK0tFIFl57mwXnRVaSUJBxwywOhfV\nngcHBhDrKQKBgQDuyclV7HkyplpxVcUmfasG6k0pkAwfnhGqO+jvGquwChcjHwVZ\n7XP+8FsQ2N2ktugtZ6fXu7hFymYSIgkNcrYHndBLxY99n9y3F+orNYUx7b0s4zw9\nWlp16l56ZdC98nmT/zKS/h2cYooK6lCwH6Mh18GDtGvgGEThFLolpv+bYQKBgGgS\nvRYh8NJbASI+X/CfmzjRWjHSqzn9qoCW2IqTSVcqACiIehHPpY0KHKaSaXZ5Zq08\nWQRMwCHZZQ+duN7HIBMg3gpPYjmz1r1h/3Qr1AGCemqp/yUNhT63Aob1I5vEh2Pp\n7E/ESudUNukvyQeD4EF3sDgOIPEY/u1qawosBEO5AoGAFZ6rHrZCbZvDnBl6ja+c\nBHRqp5pyCzVradul80OSAeXp0T7SQAASISk6/ctqCBlTdeaQy2mikxNFzv+C/EJD\n0zjrTSQh25fPD9VLKrJj4lqVlZgPvs27sQtmLOsKtOpGNi0ny/+peTKTdjQznpxu\ntSZdwXd6SwimE8rsG+yeklg=\n-----END PRIVATE KEY-----\n',
          },
          global_select_limit: '1.0',
          project_id: 'hackanoodle',
          datasets: ['dataset1', 'additional_dataset'],
        },
      },
    ],
    remote_schemas: [
      {
        name: 'remoteSchema1',
        definition: {
          url: 'https://some-graph-endpoint.com/api/graphql',
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
    ],
  },
};
