import { renderHook } from '@testing-library/react';
import { http, HttpResponse } from 'msw';
import { setupServer } from 'msw/node';
import { buildSchema, getIntrospectionQuery, graphqlSync } from 'graphql';
import {
  downloadObjectAsCsvFile,
  downloadObjectAsJsonFile,
} from '@hasura/shared/utils';
import { Metadata, MetadataTable } from '@hasura/shared/types';
import { UseRowsPropType } from '../useRows';
import { TableRow } from '../../../driver';
import { testWrapper } from '@hasura/shared/testing';
import { useExportRows } from './index';

// `getTableColumns` always calls `introspectTableScalarTypes`, which runs
// `buildClientSchema` on the full GraphQL introspection payload. That
// requires a structurally complete introspection result (all built-in
// scalar/meta types included), not just the hand-picked `Album` type, so we
// generate it from a real schema instead of hand-writing a partial one.
const mockGraphQLSchema = buildSchema(`
  type Album {
    AlbumId: Int!
    ArtistId: Int!
    Title: String!
  }

  type query_root {
    Album: [Album!]!
  }

  schema {
    query: query_root
  }
`);

const mockIntrospectionResult = graphqlSync({
  schema: mockGraphQLSchema,
  source: getIntrospectionQuery(),
}).data;

vi.mock('@hasura/shared/utils', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@hasura/shared/utils')>();
  return {
    ...actual,
    downloadObjectAsCsvFile: vi.fn(),
    downloadObjectAsJsonFile: vi.fn(),
  };
});

const baseUseExportRowsPros: UseRowsPropType = {
  source: { name: 'chinook', kind: 'postgres' },
  table: { name: 'Album', schema: 'public' },
  columns: [],
  options: {
    limit: 10,
    where: [{ AlbumId: { $gt: 4 } }],
    order_by: [{ column: 'Title', type: 'desc' }],
    offset: 15,
  },
};

describe('useExportRows', () => {
  const mockMetadata: Metadata = {
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
            } as MetadataTable,
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

  const expectedResult: TableRow[] = [
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
  ];

  const server = setupServer(
    http.post('http://localhost/v1/metadata', () => {
      return HttpResponse.json(mockMetadata, { status: 200 });
    }),
    http.post('http://localhost/v2/query', () => {
      return HttpResponse.json(expectedResult, { status: 200 });
    }),
    http.post('/v1/graphql', () => {
      return HttpResponse.json(
        {
          data: mockIntrospectionResult,
        },
        { status: 200 },
      );
    }),
  );

  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
  });
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('runs the CSV download function', async () => {
    const { result } = renderHook(() => useExportRows(), {
      wrapper: testWrapper,
    });

    await result.current.onExportRows(baseUseExportRowsPros, 'CSV');

    expect(downloadObjectAsJsonFile).not.toHaveBeenCalled();
    expect(downloadObjectAsCsvFile).toHaveBeenCalledWith(
      expect.stringContaining('export_public_Album'),
      expectedResult,
    );
  });

  it('runs the JSON download function', async () => {
    const { result } = renderHook(() => useExportRows(), {
      wrapper: testWrapper,
    });

    await result.current.onExportRows(baseUseExportRowsPros, 'JSON');

    expect(downloadObjectAsCsvFile).not.toHaveBeenCalled();
    expect(downloadObjectAsJsonFile).toHaveBeenCalledWith(
      expect.stringContaining('export_public_Album'),
      expectedResult,
    );
  });
});
