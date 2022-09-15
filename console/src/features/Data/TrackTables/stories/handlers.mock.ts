import { RunSQLResponse } from '@/features/DataSource';
import { Metadata } from '@/features/MetadataAPI';
import { rest } from 'msw';

const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
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
        ],
        configuration: {
          connection_info: {
            database_url: 'http://localhost:80/postgres',
          },
        },
      },
    ],
  },
};

const runSQLResponse: RunSQLResponse = {
  result_type: 'TuplesOk',
  result: [
    ['table_name', 'table_schema', 'table_type'],
    ['Artist', 'public', 'BASE TABLE'],
    ['Album', 'public', 'BASE TABLE'],
    ['Employee', 'public', 'BASE TABLE'],
    ['Customer', 'public', 'BASE TABLE'],
    ['Invoice', 'public', 'BASE TABLE'],
    ['InvoiceLine', 'public', 'BASE TABLE'],
    ['Track', 'public', 'BASE TABLE'],
    ['Playlist', 'public', 'BASE TABLE'],
    ['PlaylistTrack', 'public', 'BASE TABLE'],
    ['Genre', 'public', 'BASE TABLE'],
    ['MediaType', 'public', 'BASE TABLE'],
  ],
};

export const handlers = () => [
  rest.post(`http://localhost:8080/v1/metadata`, (req, res, ctx) => {
    return res(ctx.json(metadata));
  }),
  rest.post(`http://localhost:8080/v2/query`, (req, res, ctx) => {
    return res(ctx.json(runSQLResponse));
  }),
];
