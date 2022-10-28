import { rest } from 'msw';
import { Metadata } from '@/features/MetadataAPI';
import { TableInfo } from '@hasura/dc-api-types';

const baseUrl = 'http://localhost:8080';

const mockMetadata: Metadata = {
  resource_version: 14,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'sqlite_test',
        kind: 'sqlite',
        tables: [
          {
            table: ['Album'],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            db: './chinook.db',
            include_sqlite_meta_tables: false,
            tables: ['Album', 'Artist', 'Genre'],
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        sqlite: {
          uri: 'http://host.docker.internal:8100',
        },
      },
    },
  },
};

const tableInfo: TableInfo = {
  name: ['Album'],
  columns: [
    {
      name: 'AlbumId',
      type: 'number',
      nullable: false,
    },
    {
      name: 'Title',
      type: 'string',
      nullable: false,
    },
    {
      name: 'ArtistId',
      type: 'number',
      nullable: false,
    },
  ],
  primary_key: ['AlbumId'],
  foreign_keys: {
    'ArtistId->Artist.ArtistId': {
      foreign_table: ['Artist'],
      column_mapping: {
        ArtistId: 'ArtistId',
      },
    },
  },
  description:
    'CREATE TABLE [Album]\n(\n    [AlbumId] INTEGER  NOT NULL,\n    [Title] NVARCHAR(160)  NOT NULL,\n    [ArtistId] INTEGER  NOT NULL,\n    CONSTRAINT [PK_Album] PRIMARY KEY  ([AlbumId]),\n    FOREIGN KEY ([ArtistId]) REFERENCES [Artist] ([ArtistId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION\n)',
};

const introspectionResponse = {
  data: {
    __schema: {
      types: [
        {
          kind: 'OBJECT',
          name: 'Album',
          description:
            'CREATE TABLE [Album]\n(\n    [AlbumId] INTEGER  NOT NULL,\n    [Title] NVARCHAR(160)  NOT NULL,\n    [ArtistId] INTEGER  NOT NULL,\n    CONSTRAINT [PK_Album] PRIMARY KEY  ([AlbumId]),\n    FOREIGN KEY ([ArtistId]) REFERENCES [Artist] ([ArtistId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION\n)',
          fields: [
            {
              name: 'AlbumId',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'decimal',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ArtistId',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'decimal',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Title',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
      ],
    },
  },
};

const mockTableRows = {
  data: {
    Album: [
      {
        AlbumId: 1,
        Title: 'For Those About To Rock We Salute You',
        ArtistId: 1,
      },
      {
        AlbumId: 2,
        Title: 'Balls to the Wall',
        ArtistId: 2,
      },
      {
        AlbumId: 3,
        Title: 'Restless and Wild',
        ArtistId: 2,
      },
      {
        AlbumId: 4,
        Title: 'Let There Be Rock',
        ArtistId: 1,
      },
      {
        AlbumId: 5,
        Title: 'Big Ones',
        ArtistId: 3,
      },
      {
        AlbumId: 6,
        Title: 'Jagged Little Pill',
        ArtistId: 4,
      },
      {
        AlbumId: 7,
        Title: 'Facelift',
        ArtistId: 5,
      },
      {
        AlbumId: 8,
        Title: 'Warner 25 Anos',
        ArtistId: 6,
      },
      {
        AlbumId: 9,
        Title: 'Plays Metallica By Four Cellos',
        ArtistId: 7,
      },
      {
        AlbumId: 10,
        Title: 'Audioslave',
        ArtistId: 8,
      },
    ],
  },
};

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v1/metadata`, async (_req, res, ctx) => {
    const reqBody = (await _req.json()) as Record<string, any>;

    if (reqBody.type === 'export_metadata') return res(ctx.json(mockMetadata));
    if (reqBody.type === 'get_table_info') return res(ctx.json(tableInfo));

    return res(ctx.json({}));
  }),
  rest.post(`${url}/v1/graphql`, async (_req, res, ctx) => {
    const reqBody = (await _req.json()) as Record<string, any>;

    if (reqBody.operationName === 'IntrospectionQuery')
      return res(ctx.json(introspectionResponse));

    if (reqBody.operationName === 'TableRows')
      return res(ctx.json(mockTableRows));

    return res(ctx.json({}));
  }),
];
