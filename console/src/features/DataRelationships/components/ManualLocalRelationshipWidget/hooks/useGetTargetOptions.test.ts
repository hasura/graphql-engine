import { renderHook } from '@testing-library/react-hooks';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { wrapper } from '../../../../../hooks/__tests__/common/decorator';
import { Metadata, Table } from '../../../../MetadataAPI';
import { useGetTargetOptions } from './useGetTargetOptions';

describe('useGetTargetOptions', () => {
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
            {
              table: {
                name: 'Artist',
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

  const server = setupServer(
    rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
      return res(ctx.status(200), ctx.json(mockMetadata));
    })
  );

  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
  });

  it('when invoked, fetches all tables when no option excludeTable options is provided', async () => {
    const { result, waitFor } = renderHook(
      () => useGetTargetOptions('chinook'),
      { wrapper }
    );

    const expectedResult: Table[] = [
      {
        name: 'Album',
        schema: 'public',
      },
      {
        name: 'Artist',
        schema: 'public',
      },
    ];

    await waitFor(() => result.current.isSuccess);
    expect(result.current.data).toEqual(expectedResult);
  });

  it('when invoked, fetches the filtered list of tables when an option excludeTable options is provided', async () => {
    const { result, waitFor } = renderHook(
      () =>
        useGetTargetOptions('chinook', {
          name: 'Album',
          schema: 'public',
        }),
      { wrapper }
    );

    const expectedResult: Table[] = [
      {
        name: 'Artist',
        schema: 'public',
      },
    ];

    await waitFor(() => result.current.isSuccess);
    expect(result.current.data).toEqual(expectedResult);
  });
});
