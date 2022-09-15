import { renderHook } from '@testing-library/react-hooks';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { useTables, UseTablesProps } from '../hooks/useTables';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { TrackableTable } from '../types';
import { Metadata } from '../../../MetadataAPI';

const metadataStub: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'otherDataSource',
        kind: 'postgres',
        tables: [],
        configuration: {
          connection_info: {
            database_url: '',
          },
        },
      },
      {
        name: 'default',
        kind: 'postgres',
        configuration: {
          connection_info: {
            database_url: '',
          },
        },
        tables: [
          {
            table: {
              name: 'Users',
              schema: 'public',
            },
            configuration: {
              custom_root_fields: {
                select: 'foo',
              },
            },
          },
        ],
      },
    ],
  },
};

const runSqlStub = {
  result_type: 'TuplesOk',
  result: [
    ['table_name', 'table_schema', 'table_type'],
    ['Users', 'public', 'BASE TABLE'],
    ['Products', 'public', 'BASE TABLE'],
  ],
};

const server = setupServer();

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

describe('useTables', () => {
  describe('when happy path', () => {
    beforeEach(() => {
      server.use(
        rest.post('/v1/metadata', (req, res, ctx) => {
          return res(ctx.status(200), ctx.json(metadataStub));
        }),
        rest.post('/v2/query', (req, res, ctx) => {
          return res(ctx.status(200), ctx.json(runSqlStub));
        })
      );
    });

    it('returns the tracked tables', async () => {
      const { dataSourceName }: UseTablesProps = {
        dataSourceName: 'default',
      };
      const { result, waitForNextUpdate } = renderHook(
        () => useTables({ dataSourceName }),
        {
          wrapper,
        }
      );

      await waitForNextUpdate();

      const expectedResult: TrackableTable[] = [
        {
          id: 'public.Users',
          is_tracked: true,
          name: 'public.Users',
          table: { name: 'Users', schema: 'public' },
          type: 'BASE TABLE',
          configuration: {
            custom_root_fields: {
              select: 'foo',
            },
          },
        },
        {
          id: 'public.Products',
          is_tracked: false,
          name: 'public.Products',
          table: { name: 'Products', schema: 'public' },
          type: 'BASE TABLE',
        },
      ];

      expect(result.current.data).toEqual(expectedResult);
    });
  });

  describe('when metadata source is not defined', () => {
    beforeEach(() => {
      server.use(
        rest.post('/v1/metadata', (req, res, ctx) =>
          res(ctx.status(200), ctx.json({ metadata: { test: 'a' } }))
        )
      );
    });
    it('throws an error', async () => {
      const { dataSourceName }: UseTablesProps = {
        dataSourceName: 'default',
      };
      const { result, waitForNextUpdate } = renderHook(
        () => useTables({ dataSourceName }),
        { wrapper }
      );

      await waitForNextUpdate();
      expect(result.current.error).toEqual(
        Error(`useTables.metadataSource not found`)
      );
    });
  });
});
