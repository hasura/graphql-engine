import { renderHook } from '@testing-library/react-hooks';
import { UseQueryResult } from 'react-query';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { useTableColumns } from '../useTableColumns';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';

const exportMetadataMockResponse = {
  resource_version: 3,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [{ table: { schema: 'public', name: 'Album' } }],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'read-committed',
          },
        },
      },
    ],
  },
};

const queryResultMockResponse = {
  result_type: 'TuplesOk',
  result: [
    ['database', 'table_schema', 'table_name', 'column_name', 'data_type'],
    ['chinook', 'public', 'Album', 'AlbumId', 'integer'],
    ['chinook', 'public', 'Album', 'Title', 'character varying'],
    ['chinook', 'public', 'Album', 'ArtistId', 'integer'],
  ],
};

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    // return res(ctx.json(mockResponse));
    return res(
      // Send a valid HTTP status code
      ctx.status(200),
      // And a response body, if necessary
      ctx.json(exportMetadataMockResponse)
    );
  }),
  rest.post('http://localhost/v2/query', (req, res, ctx) => {
    // return res(ctx.json(mockResponse));
    return res(
      // Send a valid HTTP status code
      ctx.status(200),
      // And a response body, if necessary
      ctx.json(queryResultMockResponse)
    );
  })
);

describe('The useTableColumns hook', () => {
  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should show loading status whilst fetching data', async () => {
    const { result } = renderHook(
      () =>
        useTableColumns('chinook', {
          schema: 'public',
          name: 'Album',
        }),
      {
        wrapper,
      }
    );
    expect(
      (result.all[0] as UseQueryResult<Record<string, unknown>, Error>).status
    ).toBe('idle');
  });

  it('should load the proper data after success', async () => {
    const { result, waitFor } = renderHook(
      () =>
        useTableColumns('chinook', {
          schema: 'public',
          name: 'Album',
        }),
      {
        wrapper,
      }
    );

    await waitFor(() => result.current.isSuccess);
    expect(
      (result.current as UseQueryResult<Record<string, unknown>, Error>).status
    ).toBe('success');
    expect(result.current.data).toMatchInlineSnapshot(`
      [
        [
          "database",
          "table_schema",
          "table_name",
          "column_name",
          "data_type",
        ],
        [
          "chinook",
          "public",
          "Album",
          "AlbumId",
          "integer",
        ],
        [
          "chinook",
          "public",
          "Album",
          "Title",
          "character varying",
        ],
        [
          "chinook",
          "public",
          "Album",
          "ArtistId",
          "integer",
        ],
      ]
    `);
  });
});
