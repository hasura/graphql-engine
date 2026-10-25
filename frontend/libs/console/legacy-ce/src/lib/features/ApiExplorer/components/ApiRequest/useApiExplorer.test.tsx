/**
 * Coverage for useApiExplorer after the set-state-in-effect fix: the initial
 * `query` is now seeded synchronously by a lazy useState initializer (from
 * localStorage / table count) instead of being set from inside the mount
 * effect, while the remote `query_file` path stays asynchronous in the effect.
 * The mount effect is still mount-only (reads getHeaders/searchParams via a ref)
 * and initializes the headers.
 */
import React from 'react';
import { act, renderHook, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { vi } from 'vitest';
import { AuthContext } from '@hasura/shared/context';

const h = vi.hoisted(() => ({
  lsQuery: null as string | null,
  requestText: 'remote-query-content',
}));

vi.mock('./utils', async (importActual) => {
  const actual = await importActual<typeof import('./utils')>();
  return {
    ...actual,
    getGraphiQLQueryFromLocalStorage: () => h.lsQuery,
    getPersistedGraphiQLHeaders: () => [],
    getDefaultGraphiqlHeaders: () => [
      { key: 'content-type', value: 'application/json', selected: true },
    ],
  };
});

vi.mock('@hasura/shared/utils', async (importActual) => {
  const actual = await importActual<typeof import('@hasura/shared/utils')>();
  return {
    ...actual,
    getLSItem: () => null,
    removeLSItem: () => undefined,
    request: () =>
      Promise.resolve({ text: () => Promise.resolve(h.requestText) }),
  };
});

import useApiExplorer from './useApiExplorer';

const makeWrapper =
  (entries: string[]) =>
  ({ children }: { children: React.ReactNode }) => (
    <MemoryRouter initialEntries={entries}>
      <AuthContext.Provider
        value={
          {
            isAuthenticated: true,
            authType: 'none',
            getHeaders: () => Promise.resolve({}),
            authenticate: () => Promise.resolve(true),
            initialize: () => Promise.resolve(),
            logout: () => undefined,
          } as any
        }
      >
        {children}
      </AuthContext.Provider>
    </MemoryRouter>
  );

const renderExplorer = (numberOfTables: number, entries = ['/']) =>
  renderHook(() => useApiExplorer({ numberOfTables }), {
    wrapper: makeWrapper(entries),
  });

beforeEach(() => {
  h.lsQuery = null;
  h.requestText = 'remote-query-content';
});

describe('useApiExplorer initial query seeding', () => {
  it('shows the "no tables" message when there are no tables and nothing in LS', () => {
    const { result } = renderExplorer(0);
    expect(result.current.query).toContain('do not have any tables');
  });

  it('seeds the query from localStorage when present', () => {
    h.lsQuery = 'query { fromLS }';
    const { result } = renderExplorer(5);
    expect(result.current.query).toBe('query { fromLS }');
  });

  it('shows the fresh-graphql message when LS holds a stale "do not have" message', () => {
    h.lsQuery = '# Looks like you do not have any tables.';
    const { result } = renderExplorer(5);
    expect(result.current.query).toContain('Try out GraphQL queries here');
  });

  it('starts empty then loads a remote query_file asynchronously', async () => {
    const { result } = renderExplorer(5, ['/?query_file=https://x/q.gql']);
    expect(result.current.query).toBe('');
    await waitFor(() =>
      expect(result.current.query).toBe('remote-query-content'),
    );
  });

  it('initializes headers on mount', async () => {
    const { result } = renderExplorer(5);
    expect(result.current.headersInitialized).toBe(false);
    await waitFor(() => expect(result.current.headersInitialized).toBe(true));
    // a trailing empty placeholder header is always appended
    expect(result.current.headers.length).toBeGreaterThan(0);
  });
});

describe('useApiExplorer header changes', () => {
  it('applies a checkbox toggle to objectHeaders immediately', async () => {
    const { result } = renderExplorer(5);
    await waitFor(() => expect(result.current.headersInitialized).toBe(true));
    expect(result.current.objectHeaders).toEqual({
      'content-type': 'application/json',
    });

    act(() => {
      result.current.changeRequestHeader(
        { ...result.current.headers[0], selected: false },
        0,
      );
    });
    expect(result.current.objectHeaders).toEqual({});

    act(() => {
      result.current.changeRequestHeader(
        { ...result.current.headers[0], selected: true },
        0,
      );
    });
    expect(result.current.objectHeaders).toEqual({
      'content-type': 'application/json',
    });
  });

  it('defers key/value edits to objectHeaders until blur', async () => {
    const { result } = renderExplorer(5);
    await waitFor(() => expect(result.current.headersInitialized).toBe(true));

    act(() => {
      result.current.changeRequestHeader(
        { ...result.current.headers[0], value: 'text/plain' },
        0,
      );
    });
    expect(result.current.objectHeaders).toEqual({
      'content-type': 'application/json',
    });

    act(() => {
      result.current.handleHeaderUnfocus();
    });
    expect(result.current.objectHeaders).toEqual({
      'content-type': 'text/plain',
    });
  });
});
