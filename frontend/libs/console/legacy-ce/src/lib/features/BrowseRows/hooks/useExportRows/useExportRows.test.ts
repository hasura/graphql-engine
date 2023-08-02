import { renderHook } from '@testing-library/react-hooks';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import {
  downloadObjectAsCsvFile,
  downloadObjectAsJsonFile,
} from '../../../../components/Common/utils/export.utils';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { TableRow } from '../../../DataSource';
import { Metadata } from '../../../hasura-metadata-types';
import { useExportRows } from './useExportRows';
import { UseRowsPropType } from '../useRows';

jest.mock('../../../../components/Common/utils/export.utils', () => ({
  ...jest.requireActual('../../../../components/Common/utils/export.utils'),
  downloadObjectAsCsvFile: jest.fn(),
  downloadObjectAsJsonFile: jest.fn(),
}));

const baseUseExportRowsPros: UseRowsPropType = {
  dataSourceName: 'chinook',
  table: { name: 'Album', schema: 'public' },
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
    rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
      return res(ctx.status(200), ctx.json(mockMetadata));
    }),
    rest.post('http://localhost/v2/query', (req, res, ctx) => {
      return res(ctx.status(200), ctx.json(expectedResult));
    }),
    rest.post('/v1/graphql', (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.json({
          data: {
            __schema: {
              queryType: { name: 'query_root' },
              mutationType: { name: 'mutation_root' },
              subscriptionType: { name: 'subscription_root' },
              types: [
                {
                  kind: 'OBJECT',
                  name: 'Album',
                  description: 'asdasdads',
                  fields: [
                    {
                      name: 'AlbumId',
                      description: null,
                      args: [],
                      type: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
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
                        ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
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
        })
      );
    })
  );

  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('runs the CSV download function', async () => {
    const { result } = renderHook(() => useExportRows(baseUseExportRowsPros), {
      wrapper,
    });

    await result.current.onExportRows('CSV');

    expect(downloadObjectAsJsonFile).not.toHaveBeenCalled();
    expect(downloadObjectAsCsvFile).toHaveBeenCalledWith(
      expect.stringContaining('export_Album_'),
      expectedResult
    );
  });

  it('runs the JSON download function', async () => {
    const { result } = renderHook(() => useExportRows(baseUseExportRowsPros), {
      wrapper,
    });

    await result.current.onExportRows('JSON');

    expect(downloadObjectAsCsvFile).not.toHaveBeenCalled();
    expect(downloadObjectAsJsonFile).toHaveBeenCalledWith(
      expect.stringContaining('export_Album_'),
      expectedResult
    );
  });
});
