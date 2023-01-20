import { RunSQLResponse } from '@/features/DataSource';
import { areTablesEqual } from '@/features/hasura-metadata-api';
import { Metadata } from '@/features/hasura-metadata-types';
import { TMigration } from '@/features/MetadataAPI';
import { rest } from 'msw';

function initialMetadata(): Metadata {
  return {
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
}

type Table = { schema: string; name: string };

let metadata = initialMetadata();

function trackTable(table: Table) {
  metadata.metadata.sources[0].tables.push({ table });
}

function untrackTable(table: Table) {
  metadata.metadata.sources[0].tables =
    metadata.metadata.sources[0].tables.filter(
      t => !areTablesEqual(t.table, table)
    );
}

export const resetMetadata = () => {
  metadata = initialMetadata();
};

function isTrackOrUntrackTable(body: any) {
  return body.type === 'bulk';
}

function isTrackTable(arg: any) {
  return arg.type === 'postgres_track_table';
}

function isUntrackTable(arg: any) {
  return arg.type === 'postgres_untrack_table';
}

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
  rest.post(`http://localhost:8080/v1/metadata`, async (req, res, ctx) => {
    const body = (await req.json()) as TMigration['query'];
    if (isTrackOrUntrackTable(body)) {
      body.args.forEach((arg: any) => {
        if (isTrackTable(arg)) {
          trackTable(arg.args.table);
        }
        if (isUntrackTable(arg)) {
          untrackTable(arg.args.table);
        }
      });
    }
    return res(ctx.json({ ...metadata }));
  }),
  rest.post(`http://localhost:8080/v2/query`, (req, res, ctx) => {
    return res(ctx.json(runSQLResponse));
  }),
];
