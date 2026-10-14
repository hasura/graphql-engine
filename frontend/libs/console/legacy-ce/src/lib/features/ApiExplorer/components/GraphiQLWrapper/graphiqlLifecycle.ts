/**
 * Small, testable pieces of the GraphiQL 5 wrapper lifecycle:
 *  - `useGraphqlWsClient`: a graphql-ws subscription client that is created and
 *    DISPOSED by the same effect, so it survives React StrictMode and is torn
 *    down when its (url, headers) change or the component unmounts (GraphiQL v5
 *    upgrade compatibility — the previous code created the client inside the
 *    fetcher useMemo and never disposed it, leaking a websocket client per
 *    reconfig).
 *  - `planExternalQuerySync`: the pure decision for syncing an external query
 *    (e.g. `query_file`) into the Monaco editor without clobbering user edits.
 */
import { useEffect, useState, useSyncExternalStore } from 'react';
import { createClient, Client } from 'graphql-ws';

/**
 * A tiny external store holding the "current" graphql-ws client. The client is
 * an external resource (opens a websocket), so we synchronize it to React via
 * `useSyncExternalStore` instead of `useState`: the effect that owns the client
 * lifecycle updates the store, and the store notifies subscribers to re-render.
 * This keeps the client creation/disposal entirely inside one effect (the
 * StrictMode-safe shape) without calling a React state setter from the effect.
 */
function createWsClientStore() {
  let client: Client | null = null;
  const listeners = new Set<() => void>();

  return {
    subscribe(listener: () => void) {
      listeners.add(listener);
      return () => {
        listeners.delete(listener);
      };
    },
    getSnapshot() {
      return client;
    },
    setClient(next: Client | null) {
      client = next;
      listeners.forEach((listener) => listener());
    },
    /** Clear only if we still own `expected` (guards stale cleanup). */
    clearClient(expected: Client) {
      if (client === expected) {
        this.setClient(null);
      }
    },
  };
}

/**
 * Owns a graphql-ws client for the current (wsUrl, headers). Returns `null` on
 * the first render (before the effect runs); the fetcher must treat a missing
 * client as "no subscriptions yet" and recreate once it is ready.
 *
 * The client is created AND disposed inside one effect. That is what makes it
 * StrictMode-safe: React 18/19 StrictMode runs effects mount -> cleanup ->
 * mount, so the first (throwaway) client is disposed by its own cleanup and the
 * component ends up holding the second, live client. Creating the client in
 * `useMemo` and disposing it in an effect (the previous shape) instead leaves
 * the single retained memo client permanently disposed after that first cleanup
 * — `dispose()` is terminal, so the websocket could never reconnect.
 */
export function useGraphqlWsClient(
  wsUrl: string,
  headers: Record<string, string>,
): Client | null {
  // One stable store instance per hook instance (lazy-initialized once).
  const [store] = useState(createWsClientStore);

  // `headers` is typically a fresh object every render; key the effect off its
  // serialized VALUE so only a real url/headers change recreates the client.
  // Re-deriving the object from that same string inside the effect keeps the
  // effect's dependencies honest (`headersKey` is both the dep and the source
  // of the headers used) without depending on the per-render object identity.
  const headersKey = JSON.stringify(headers);

  useEffect(() => {
    // The client is created AND disposed inside this one effect, which is what
    // makes it StrictMode-safe: React runs effects mount -> cleanup -> mount, so
    // the first (throwaway) client is disposed by its own cleanup and the store
    // ends up holding the second, live client.
    const activeClient = createClient({
      url: wsUrl,
      connectionParams: { headers: JSON.parse(headersKey) },
      lazy: true,
    });
    store.setClient(activeClient);
    return () => {
      // Clear only if we still own it (guards against a stale cleanup nulling a
      // newer client during rapid reconfig), then dispose.
      store.clearClient(activeClient);
      activeClient.dispose();
    };
  }, [wsUrl, headersKey, store]);

  return useSyncExternalStore(store.subscribe, store.getSnapshot);
}

export interface ExternalQuerySyncInput {
  externalQuery: string;
  lastExternal: string;
  editorValue: string | undefined; // undefined => editor not ready
}

export interface ExternalQuerySyncPlan {
  /** Call queryEditor.setValue(value) when true. */
  setValue: boolean;
  value?: string;
  /** New value to store as `lastExternal` (undefined => leave unchanged). */
  nextLast?: string;
}

/**
 * Decide how to reconcile an external query with the editor:
 *  - editor not ready            -> do nothing (retry when it mounts)
 *  - no external change          -> do nothing
 *  - editor already has it       -> record it (user's own edit echoing back)
 *  - genuinely new external value-> setValue + record it
 */
export function planExternalQuerySync(
  input: ExternalQuerySyncInput,
): ExternalQuerySyncPlan {
  const { externalQuery, lastExternal, editorValue } = input;
  if (editorValue === undefined) {
    return { setValue: false };
  }
  if (externalQuery === lastExternal) {
    return { setValue: false };
  }
  if (editorValue === externalQuery) {
    return { setValue: false, nextLast: externalQuery };
  }
  return { setValue: true, value: externalQuery, nextLast: externalQuery };
}
