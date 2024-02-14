import { RunSQLResponse } from '../../../DataSource';
import { areTablesEqual } from '../../../hasura-metadata-api';
import { Metadata } from '../../../hasura-metadata-types';
import { TMigration } from '../../../MetadataAPI';
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

// function isTrackOrUntrackTable(body: any) {
//   return (
//     body.type === 'postgres_track_tables' ||
//     body.type === 'postgres_track_tables'
//   );
// }

function isTrackTable(type: string) {
  return type === 'postgres_track_tables';
}

function isUntrackTable(type: any) {
  return type === 'postgres_untrack_tables';
}

const runSQLResponse = (size = 100): RunSQLResponse => ({
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
    ['Country', 'public', 'BASE TABLE'],
    ['State', 'public', 'BASE TABLE'],
    ['City', 'public', 'BASE TABLE'],
    ['CustomerList', 'public', 'VIEW'],
    ['InvoiceLineView', 'public', 'VIEW'],
    ['InvoiceView', 'public', 'VIEW'],
    ['TrackView', 'public', 'VIEW'],
    ...createTables(size),
  ],
});

function createTables(count: number) {
  const tables = [];
  for (let i = 0; i < count; i++) {
    tables.push([`table_${i}`, 'public', 'BASE TABLE']);
  }
  return tables;
}

export const handlers = (amountOfTables = 1700) => [
  rest.post(`http://localhost:8080/v1/metadata`, async (req, res, ctx) => {
    const body = (await req.json()) as TMigration['query'];
    if (isTrackTable(body.type)) {
      body.args.tables.forEach((table: any) => trackTable(table.table));
    } else if (isUntrackTable(body.type)) {
      body.args.tables.forEach((table: any) => untrackTable(table.table));
    }

    return res(ctx.json({ ...metadata }));
  }),
  rest.post(`http://localhost:8080/v2/query`, async (req, res, ctx) => {
    const body = (await req.json()) as TMigration['query'];

    console.log('!!1', body);

    return res(ctx.json([runSQLResponse(amountOfTables), []]));
  }),
];
