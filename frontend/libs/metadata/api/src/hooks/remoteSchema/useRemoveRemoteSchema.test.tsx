import { renderHook, waitFor } from '@testing-library/react';
import { setupServer } from 'msw/node';
import { http } from 'msw';
import { handlers, testWrapper } from '@hasura/shared/testing';
import { useRemoveRemoteSchema } from './useRemoveRemoteSchema';

// HTTP-boundary tests: assert the metadata request contract sent to
// `POST /v1/metadata`, mocked with the shared MSW `handlers()`. These lock in
// that `cascade` is an explicit opt-in: it is only forwarded to the server when
// a caller sets it to `true`, so the server keeps reporting dependent objects
// by default.
let capturedBodies: any[] = [];

const captureHandler = http.post('*/v1/metadata', async ({ request }) => {
  const body = (await request.clone().json()) as any;
  if (body?.type !== 'export_metadata') {
    capturedBodies.push(body);
  }
  return undefined;
});

const server = setupServer(captureHandler, ...handlers());

describe('useRemoveRemoteSchema (HTTP boundary)', () => {
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

  it('omits cascade when called with just the remote schema name', async () => {
    const { result } = renderHook(() => useRemoveRemoteSchema(), {
      wrapper: testWrapper,
    });

    await result.current.mutate('my_remote_schema');

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0]).toEqual({
      type: 'remove_remote_schema',
      args: { name: 'my_remote_schema' },
    });
    expect(capturedBodies[0].args).not.toHaveProperty('cascade');
  });

  it('omits cascade by default when passed an options object', async () => {
    const { result } = renderHook(() => useRemoveRemoteSchema(), {
      wrapper: testWrapper,
    });

    await result.current.mutate({ name: 'my_remote_schema' });

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0].args).not.toHaveProperty('cascade');
  });

  it('does not send cascade when cascade is explicitly false', async () => {
    const { result } = renderHook(() => useRemoveRemoteSchema(), {
      wrapper: testWrapper,
    });

    await result.current.mutate({ name: 'my_remote_schema', cascade: false });

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0].args).not.toHaveProperty('cascade');
  });

  it('forwards cascade: true only when explicitly opted in', async () => {
    const { result } = renderHook(() => useRemoveRemoteSchema(), {
      wrapper: testWrapper,
    });

    await result.current.mutate({ name: 'my_remote_schema', cascade: true });

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0]).toEqual({
      type: 'remove_remote_schema',
      args: { name: 'my_remote_schema', cascade: true },
    });
  });
});
