/**
 * Regression coverage for the GraphiQL 5 wrapper lifecycle helpers:
 *  - the graphql-ws client is created/disposed by a single effect, so it
 *    survives React StrictMode (returns a LIVE client, not a disposed one),
 *    disposes on unmount, and recreates on url/headers VALUE changes while
 *    staying stable across header objects with equal values;
 *  - the external-query sync never clobbers the user's own edits.
 */
import { renderHook } from '@testing-library/react';
import { StrictMode } from 'react';
import { vi } from 'vitest';

interface MockClient {
  id: number;
  disposed: boolean;
  opts: { url?: string; lazy?: boolean; connectionParams?: unknown };
  dispose: ReturnType<typeof vi.fn>;
  subscribe: ReturnType<typeof vi.fn>;
  on: ReturnType<typeof vi.fn>;
  terminate: ReturnType<typeof vi.fn>;
}

let createdClients: MockClient[] = [];
let idCounter = 0;

const createClientMock = vi.fn((opts: MockClient['opts']): MockClient => {
  const client: MockClient = {
    id: (idCounter += 1),
    disposed: false,
    opts,
    dispose: vi.fn(() => {
      client.disposed = true;
    }),
    // graphql-ws forbids using a client after dispose(); model that so a test
    // can prove the returned client is still usable.
    subscribe: vi.fn(() => {
      if (client.disposed) throw new Error('subscribe() after dispose()');
      return () => undefined;
    }),
    on: vi.fn(),
    terminate: vi.fn(),
  };
  createdClients.push(client);
  return client;
});

vi.mock('graphql-ws', () => ({
  createClient: (opts: MockClient['opts']) => createClientMock(opts),
}));

import { useGraphqlWsClient, planExternalQuerySync } from './graphiqlLifecycle';

const asMock = (c: unknown) => c as unknown as MockClient;

beforeEach(() => {
  createdClients = [];
  idCounter = 0;
  createClientMock.mockClear();
});

describe('useGraphqlWsClient', () => {
  it('creates a lazy client with the endpoint + header connectionParams', () => {
    const headers = { 'x-hasura-admin-secret': 's' };
    const { result } = renderHook(() =>
      useGraphqlWsClient('ws://localhost:8080/v1/graphql', headers),
    );
    expect(createClientMock).toHaveBeenCalledTimes(1);
    expect(asMock(result.current).opts).toMatchObject({
      url: 'ws://localhost:8080/v1/graphql',
      lazy: true,
      connectionParams: { headers },
    });
    expect(asMock(result.current).disposed).toBe(false);
  });

  it('returns a LIVE (non-disposed) client under React StrictMode', () => {
    const { result } = renderHook(
      () => useGraphqlWsClient('ws://a/graphql', {}),
      { wrapper: StrictMode },
    );
    // StrictMode mounts twice: a throwaway client is created then disposed, and
    // the component is left holding the second, live one.
    expect(createClientMock).toHaveBeenCalledTimes(2);
    expect(createdClients.filter((c) => c.disposed)).toHaveLength(1);
    expect(result.current).not.toBeNull();
    expect(asMock(result.current).disposed).toBe(false);
    // The live client is still usable; the disposed throwaway is not.
    expect(() => asMock(result.current).subscribe()).not.toThrow();
    const disposedOne = createdClients.find((c) => c.disposed);
    expect(() => disposedOne?.subscribe()).toThrow();
  });

  it('disposes the client on unmount', () => {
    const { result, unmount } = renderHook(() =>
      useGraphqlWsClient('ws://a/graphql', {}),
    );
    const client = asMock(result.current);
    expect(client.disposed).toBe(false);
    unmount();
    expect(client.disposed).toBe(true);
  });

  it('disposes the previous client and recreates when the url changes', () => {
    const { result, rerender, unmount } = renderHook(
      ({ url }) => useGraphqlWsClient(url, {}),
      { initialProps: { url: 'ws://graphql/a' } },
    );
    const first = asMock(result.current);
    expect(createClientMock).toHaveBeenCalledTimes(1);

    rerender({ url: 'ws://relay/b' });
    const second = asMock(result.current);
    expect(createClientMock).toHaveBeenCalledTimes(2);
    expect(first.disposed).toBe(true); // old disposed
    expect(second.disposed).toBe(false); // new is live
    expect(second.id).not.toBe(first.id);

    unmount();
    expect(second.disposed).toBe(true);
  });

  it('recreates when the headers VALUE changes', () => {
    const { result, rerender } = renderHook(
      ({ headers }) => useGraphqlWsClient('ws://a/graphql', headers),
      { initialProps: { headers: { authorization: 'Bearer one' } } },
    );
    const first = asMock(result.current);

    rerender({ headers: { authorization: 'Bearer two' } });
    const second = asMock(result.current);
    expect(createClientMock).toHaveBeenCalledTimes(2);
    expect(first.disposed).toBe(true);
    expect(second.opts.connectionParams).toEqual({
      headers: { authorization: 'Bearer two' },
    });
  });

  it('does NOT recreate when a new headers object has equal values', () => {
    const { rerender } = renderHook(
      ({ headers }) => useGraphqlWsClient('ws://a/graphql', headers),
      { initialProps: { headers: { authorization: 'Bearer one' } } },
    );
    expect(createClientMock).toHaveBeenCalledTimes(1);
    // New object, identical values -> same serialized key -> no recreate.
    rerender({ headers: { authorization: 'Bearer one' } });
    expect(createClientMock).toHaveBeenCalledTimes(1);
  });
});

describe('planExternalQuerySync', () => {
  it('does nothing while the editor is not ready', () => {
    expect(
      planExternalQuerySync({
        externalQuery: 'q',
        lastExternal: '',
        editorValue: undefined,
      }),
    ).toEqual({ setValue: false });
  });

  it('does nothing when the external query has not changed', () => {
    expect(
      planExternalQuerySync({
        externalQuery: 'same',
        lastExternal: 'same',
        editorValue: 'anything',
      }),
    ).toEqual({ setValue: false });
  });

  it("records but does NOT setValue when the editor already matches (user's own edit echo)", () => {
    expect(
      planExternalQuerySync({
        externalQuery: 'typed by user',
        lastExternal: 'old',
        editorValue: 'typed by user',
      }),
    ).toEqual({ setValue: false, nextLast: 'typed by user' });
  });

  it('setValue on a genuinely new external query (e.g. query_file load)', () => {
    expect(
      planExternalQuerySync({
        externalQuery: 'from file',
        lastExternal: '',
        editorValue: 'user draft',
      }),
    ).toEqual({ setValue: true, value: 'from file', nextLast: 'from file' });
  });
});
