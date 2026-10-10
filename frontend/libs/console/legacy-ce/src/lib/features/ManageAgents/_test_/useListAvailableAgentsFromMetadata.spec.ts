import { renderHook, waitFor } from '@testing-library/react';
import { http, HttpResponse } from 'msw';
import { setupServer } from 'msw/node';
import { testWrapper as wrapper } from '@hasura/shared/testing';
import { useListAvailableAgentsFromMetadata } from '../hooks';
import { Metadata } from '@hasura/shared/types';
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
  http.post('http://localhost/v1/metadata', () => {
    return HttpResponse.json(metadata, { status: 200 });
  }),
);

describe('useListAvailableAgentsFromMetadata tests: ', () => {
  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
  });

  it('lists all the dc agents from metadata', async () => {
    const { result } = renderHook(() => useListAvailableAgentsFromMetadata(), {
      wrapper,
    });

    const expectedResult: DcAgent[] = [
      { name: 'csv', uri: 'http://host.docker.internal:8101' },
      { name: 'sqlite', uri: 'http://host.docker.internal:8100' },
    ];

    await waitFor(() => expect(result.current.isSuccess).toBeTruthy());

    expect(result.current.data).toEqual(expectedResult);
  });
});
