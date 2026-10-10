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
import { useEffect, useState } from 'react';
import { createClient, Client } from 'graphql-ws';

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
  const [client, setClient] = useState<Client | null>(null);

  // `headers` is typically a fresh object every render; key the effect off its
  // serialized VALUE so only a real url/headers change recreates the client.
  const headersKey = JSON.stringify(headers);

  useEffect(() => {
    // The effect only re-runs when `headersKey` (the value) changes, and this
    // closure captures the `headers` object from exactly that render — so it is
    // the latest headers without writing a ref during render.
    const activeClient = createClient({
      url: wsUrl,
      connectionParams: { headers },
      lazy: true,
    });
    setClient(activeClient);
    return () => {
      // Only clear state if we still own it (guards against a stale cleanup
      // nulling a newer client during rapid reconfig).
      setClient((current) => (current === activeClient ? null : current));
      activeClient.dispose();
    };
    // headersKey is the value-stable stand-in for the headers object.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [wsUrl, headersKey]);

  return client;
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
