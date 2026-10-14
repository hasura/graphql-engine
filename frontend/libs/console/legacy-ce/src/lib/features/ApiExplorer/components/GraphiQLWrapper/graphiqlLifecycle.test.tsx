/**
 * Coverage for useGraphqlWsClient after its set-state-in-effect rewrite: the
 * graphql-ws client lifecycle is now synchronized to React via an external
 * store (useSyncExternalStore) instead of setState inside the effect. The
 * observable contract must be unchanged: null before the effect, then the
 * client; a new client (old disposed) only when the url or headers VALUE
 * changes; disposal on unmount; and no runaway re-creation on identity-only
 * header churn.
 */
import { renderHook } from '@testing-library/react';
import { vi } from 'vitest';

const h = vi.hoisted(() => ({ created: [] as { opts: any; dispose: any }[] }));

vi.mock('graphql-ws', () => ({
  createClient: (opts: any) => {
    const client = { opts, dispose: vi.fn() };
    h.created.push(client);
    return client;
  },
}));

import { useGraphqlWsClient } from './graphiqlLifecycle';

beforeEach(() => {
  h.created = [];
});

const run = (initialProps: { url: string; headers: Record<string, string> }) =>
  renderHook(({ url, headers }) => useGraphqlWsClient(url, headers), {
    initialProps,
  });

describe('useGraphqlWsClient', () => {
  it('creates a client on mount and returns it (after the effect)', () => {
    const { result } = run({ url: 'ws://a', headers: { k: '1' } });
    expect(h.created).toHaveLength(1);
    expect(result.current).toBe(h.created[0]);
    // headers are reconstructed from the serialized value
    expect(h.created[0].opts.connectionParams.headers).toEqual({ k: '1' });
    expect(h.created[0].opts.url).toBe('ws://a');
  });

  it('does NOT recreate the client when headers change identity but not value', () => {
    const { rerender } = run({ url: 'ws://a', headers: { k: '1' } });
    rerender({ url: 'ws://a', headers: { k: '1' } }); // new object, same value
    rerender({ url: 'ws://a', headers: { k: '1' } });
    expect(h.created).toHaveLength(1);
    expect(h.created[0].dispose).not.toHaveBeenCalled();
  });

  it('recreates (and disposes the old) when the headers value changes', () => {
    const { result, rerender } = run({ url: 'ws://a', headers: { k: '1' } });
    const first = h.created[0];
    rerender({ url: 'ws://a', headers: { k: '2' } });
    expect(h.created).toHaveLength(2);
    expect(first.dispose).toHaveBeenCalledTimes(1);
    expect(result.current).toBe(h.created[1]);
  });

  it('recreates when the url changes', () => {
    const { rerender } = run({ url: 'ws://a', headers: { k: '1' } });
    rerender({ url: 'ws://b', headers: { k: '1' } });
    expect(h.created).toHaveLength(2);
    expect(h.created[0].dispose).toHaveBeenCalledTimes(1);
  });

  it('disposes the client on unmount', () => {
    const { unmount } = run({ url: 'ws://a', headers: { k: '1' } });
    const client = h.created[0];
    unmount();
    expect(client.dispose).toHaveBeenCalledTimes(1);
  });
});
