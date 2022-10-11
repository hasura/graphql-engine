import { TableRow } from '@/features/DataSource';
import { Metadata } from '@/features/MetadataAPI';
import { rest } from 'msw';
import { setupServer } from 'msw/node';

export const mockMetadata: Metadata = {
  resource_version: 54,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'Album',
              schema: 'public',
            },
          },
        ],
        configuration: {
          connection_info: {
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
      },
    ],
  },
};

export const postgresTableMockData: TableRow[] = [
  {
    AlbumId: 225,
    Title: 'Volume Dois',
    ArtistId: 146,
  },
  {
    AlbumId: 275,
    Title: 'Vivaldi: The Four Seasons',
    ArtistId: 209,
  },
  {
    AlbumId: 114,
    Title: 'Virtual XI',
    ArtistId: 90,
  },
  {
    AlbumId: 52,
    Title: 'Vinícius De Moraes - Sem Limite',
    ArtistId: 70,
  },
  {
    AlbumId: 247,
    Title: 'Vinicius De Moraes',
    ArtistId: 72,
  },
  {
    AlbumId: 67,
    Title: "Vault: Def Leppard's Greatest Hits",
    ArtistId: 78,
  },
  {
    AlbumId: 245,
    Title: 'Van Halen III',
    ArtistId: 152,
  },
  {
    AlbumId: 244,
    Title: 'Van Halen',
    ArtistId: 152,
  },
  {
    AlbumId: 92,
    Title: 'Use Your Illusion II',
    ArtistId: 88,
  },
  {
    AlbumId: 91,
    Title: 'Use Your Illusion I',
    ArtistId: 88,
  },
];

export const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    return res(ctx.status(200), ctx.json(mockMetadata));
  }),
  rest.post('http://localhost/v2/query', (req, res, ctx) => {
    return res(ctx.status(200), ctx.json(postgresTableMockData));
  })
);
