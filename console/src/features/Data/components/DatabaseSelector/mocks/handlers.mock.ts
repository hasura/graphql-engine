import { rest } from 'msw';

export const handlers = () => [
  rest.post(`http://localhost:8080/v1/metadata`, (req, res, ctx) => {
    return res(
      ctx.json({
        resource_version: 30,
        metadata: {
          version: 3,
          sources: [
            {
              name: 'bigquery_test',
              kind: 'bigquery',
              tables: [
                {
                  table: {
                    dataset: 'sensei',
                    name: 'table1',
                  },
                },
              ],
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
                    name: 'ax',
                  },
                },
                {
                  table: {
                    schema: 'public',
                    name: 'ideav',
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
        },
      })
    );
  }),
];
