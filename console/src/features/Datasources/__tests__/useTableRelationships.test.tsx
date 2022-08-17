import { renderHook } from '@testing-library/react-hooks';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import React from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { Provider } from 'react-redux';
import { useTableRelationships } from '..';
import { store } from '../../../store';

const queryClient = new QueryClient();
const wrapper = ({ children }: { children: React.ReactNode }) => (
  <Provider store={store}>
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  </Provider>
);

const export_metadata_response_to_get_source_kind = {
  resource_version: 1,
  metadata: {
    version: 1,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
      },
      {
        name: 'bikes',
        kind: 'mssql',
      },
    ],
  },
};

const sample_response_for_pg_run_sql = {
  result_type: 'TuplesOk',
  result: [
    ['source_table', 'source_column', 'target_table', 'target_column'],
    ['"Track"', '"AlbumId"', '"Album"', '"AlbumId"'],
    ['"Album"', '"ArtistId"', '"Artist"', '"ArtistId"'],
  ],
};

const sample_response_for_mssql_run_sql = {
  result_type: 'TuplesOk',
  result: [
    ['table_name', 'ref_table', 'column_mapping'],
    [
      'products',
      'categories',
      '[{"column":"category_id","referenced_column":"category_id"}]',
    ],
    [
      'products',
      'brands',
      '[{"column":"brand_id","referenced_column":"brand_id"}]',
    ],
    [
      'order_items',
      'products',
      '[{"column":"product_id","referenced_column":"product_id"}]',
    ],
    [
      'stocks',
      'products',
      '[{"column":"product_id","referenced_column":"product_id"}]',
    ],
  ],
};

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    if (
      (req.body as { type: string; args: Record<string, any> }).type ===
      'export_metadata'
    ) {
      return res(
        ctx.status(200),
        ctx.json(export_metadata_response_to_get_source_kind)
      );
    }
    return Promise.resolve();
  }),
  rest.post('http://localhost/v2/query', (req, res, ctx) => {
    if (
      (req.body as { type: string; args: Record<string, any> }).type ===
      'mssql_run_sql'
    ) {
      return res(ctx.status(200), ctx.json(sample_response_for_mssql_run_sql));
    }

    return res(ctx.status(200), ctx.json(sample_response_for_pg_run_sql));
  })
);

describe('@features/Datasources/useTableRelationships hook', () => {
  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should return success when all the correct parameters are passed for a postgres table', async () => {
    const { result, waitFor } = renderHook(
      () =>
        useTableRelationships({
          target: {
            database: 'chinook',
            table: 'Album',
            schema: 'public',
          },
        }),
      { wrapper }
    );

    await waitFor(() => result.current.isSuccess);
    expect(result.current.data).toMatchSnapshot();
  });

  it('should return success when all the correct parameters are passed for a mssql table', async () => {
    const { result, waitFor } = renderHook(
      () =>
        useTableRelationships({
          target: {
            database: 'bikes',
            table: 'products',
            schema: 'production',
          },
        }),
      { wrapper }
    );

    await waitFor(() => result.current.isSuccess);
    expect(result.current.data).toMatchSnapshot();
  });
});
