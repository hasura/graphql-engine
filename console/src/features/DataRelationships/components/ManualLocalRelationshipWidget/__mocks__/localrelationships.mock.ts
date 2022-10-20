import { rest } from 'msw';
import { Metadata } from '@/features/MetadataAPI';
import { TableInfo } from '@hasura/dc-api-types';

export const schemaList = {
  result_type: 'TuplesOk',
  result: [['schema_name'], ['public'], ['default']],
};

const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'sqlite_test',
        kind: 'sqlite',
        tables: [{ table: ['Album'] }, { table: ['Artist'] }],
        configuration: {
          foo: 'bar',
        },
      },
    ],
  },
};

const AlbumTableResponse: TableInfo = {
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

const ArtistTableRespone: TableInfo = {
  name: ['Artist'],
  columns: [
    {
      name: 'ArtistId',
      type: 'number',
      nullable: false,
    },
    {
      name: 'Name',
      type: 'string',
      nullable: true,
    },
  ],
  primary_key: ['ArtistId'],
  description:
    'CREATE TABLE [Artist]\n(\n    [ArtistId] INTEGER  NOT NULL,\n    [Name] NVARCHAR(120),\n    CONSTRAINT [PK_Artist] PRIMARY KEY  ([ArtistId])\n)',
};

export const introspectionQueryResponse = {
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
        {
          kind: 'OBJECT',
          name: 'Artist',
          description:
            'CREATE TABLE [Artist]\n(\n    [ArtistId] INTEGER  NOT NULL,\n    [Name] NVARCHAR(120),\n    CONSTRAINT [PK_Artist] PRIMARY KEY  ([ArtistId])\n)',
          fields: [
            {
              name: 'ArtistId',
              description: null,
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
              name: 'Name',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
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

export const handlers = (url = 'http://localhost:8080') => [
  rest.post(`${url}/v1/metadata`, async (_req, res, ctx) => {
    const body = await _req.json();
    if (body.type === 'get_table_info' && body.args.table.includes('Album'))
      return res(ctx.json(AlbumTableResponse));
    if (body.type === 'get_table_info' && body.args.table.includes('Artist'))
      return res(ctx.json(ArtistTableRespone));
    return res(ctx.json(metadata));
  }),
  rest.post(`${url}/v1/graphql`, async (_req, res, ctx) => {
    const body = await _req.json();
    if (body.operationName === 'IntrospectionQuery')
      return res(ctx.json(introspectionQueryResponse));
    return res(ctx.json({}));
  }),
];
