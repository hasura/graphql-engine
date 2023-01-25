import { renderHook } from '@testing-library/react-hooks';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { wrapper } from '../../../hooks/__tests__/common/decorator';
import { useListAvailableAgentsFromMetadata } from '../hooks';
import { Metadata } from '../../hasura-metadata-types';
import { DcAgent } from '../types';

const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [],
    backend_configs: {
      dataconnector: {
        sqlite: {
          uri: 'http://host.docker.internal:8100',
        },
        csv: {
          uri: 'http://host.docker.internal:8101',
        },
      },
    },
  },
};

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    return res(ctx.status(200), ctx.json(metadata));
  })
);

describe('useListAvailableAgentsFromMetadata tests: ', () => {
  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
  });

  it('lists all the dc agents from metadata', async () => {
    const { result, waitFor } = renderHook(
      () => useListAvailableAgentsFromMetadata(),
      { wrapper }
    );

    const expectedResult: DcAgent[] = [
      { name: 'csv', url: 'http://host.docker.internal:8101' },
      { name: 'sqlite', url: 'http://host.docker.internal:8100' },
    ];

    await waitFor(() => result.current.isSuccess);

    expect(result.current.data).toEqual(expectedResult);
  });
});
