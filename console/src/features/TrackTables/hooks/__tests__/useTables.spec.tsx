import React from 'react';
import { renderHook } from '@testing-library/react-hooks';
import { QueryClient, QueryClientProvider } from 'react-query';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { useTables, UseTablesProps, TrackableTable } from '../useTables';
import { RunSQLResponse } from '../../../Datasources/types';
import { Metadata } from '../../../../features/DataSource/types';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
    },
  },
});
const createWrapper =
  () =>
  ({ children }: { children: React.ReactNode }) =>
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>;

const metadataStub: Metadata = {
  version: 3,
  sources: [
    {
      name: 'otherDataSource',
      kind: 'postgres',
      tables: [],
    },
    {
      name: 'default',
      kind: 'postgres',
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
};

const runSqlStub: RunSQLResponse = {
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
      const dataSource: UseTablesProps['dataSource'] = {
        name: 'default',
      };
      const { result, waitForNextUpdate } = renderHook(
        () => useTables({ dataSource }),
        {
          wrapper: createWrapper(),
        }
      );

      await waitForNextUpdate();

      const expectedResult: TrackableTable[] = [
        {
          id: '1',
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
          id: '2',
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
          res(ctx.status(200), ctx.json({ test: 'a' }))
        )
      );
    });
    it('throws an error', async () => {
      const dataSource: UseTablesProps['dataSource'] = {
        name: 'default',
      };
      const { result, waitForNextUpdate } = renderHook(
        () => useTables({ dataSource }),
        { wrapper: createWrapper() }
      );

      await waitForNextUpdate();

      expect(result.current.error).toEqual(
        Error(`useTables.metadataSource not found`)
      );
    });
  });
});
