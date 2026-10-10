import { renderHook, waitFor } from '@testing-library/react';
import { setupServer } from 'msw/node';
import { http } from 'msw';
import { handlers, testWrapper } from '@hasura/shared/testing';
import { useAddInsecureDomain } from './useAddInsecureDomain';
import { useDeleteInsecureDomain } from './useDeleteInsecureDomain';

// HTTP-boundary tests: assert the metadata request contract sent to
// `POST /v1/metadata`, mocked with the shared MSW v2 `handlers()`. Not a
// live graphql-engine integration test.
let capturedBodies: any[] = [];

const captureHandler = http.post('*/v1/metadata', async ({ request }) => {
  const body = (await request.clone().json()) as any;
  if (body?.type !== 'export_metadata') {
    capturedBodies.push(body);
  }
  return undefined;
});

const server = setupServer(captureHandler, ...handlers());

describe('useAddInsecureDomain / useDeleteInsecureDomain (HTTP boundary)', () => {
  beforeAll(() => server.listen());
  beforeEach(() => {
    capturedBodies = [];
    vitest.spyOn(console, 'error').mockImplementation(() => null);
  });
  afterEach(() => {
    vitest.spyOn(console, 'error').mockRestore();
    server.resetHandlers(captureHandler, ...handlers());
  });
  afterAll(() => server.close());

  it('useAddInsecureDomain POSTs an add_host_to_tls_allowlist request', async () => {
    const { result } = renderHook(() => useAddInsecureDomain(), {
      wrapper: testWrapper,
    });

    await result.current('example.com', '8080');

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0]).toEqual({
      type: 'add_host_to_tls_allowlist',
      args: {
        host: 'example.com',
        permissions: ['self-signed'],
        suffix: '8080',
      },
    });
  });

  it('useDeleteInsecureDomain POSTs a drop_host_from_tls_allowlist request', async () => {
    const { result } = renderHook(() => useDeleteInsecureDomain(), {
      wrapper: testWrapper,
    });

    await result.current('example.com', '8080');

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0]).toEqual({
      type: 'drop_host_from_tls_allowlist',
      args: { host: 'example.com', suffix: '8080' },
    });
  });
});
