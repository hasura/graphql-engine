// @vitest-environment jsdom
// NOTE: `shared-hooks` is a JSX-free package (its tsconfig does not enable
// `jsx`), so this test builds its React trees with `React.createElement`
// instead of JSX to avoid introducing TS17004 diagnostics.
import React, { StrictMode } from 'react';
import {
  render,
  renderHook,
  screen,
  act,
  waitFor,
} from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter, useLocation } from 'react-router';
import { describe, it, expect, vi, beforeEach, type Mock } from 'vitest';
import {
  AuthContext,
  AppContext,
  defaultAppState,
  type AuthService,
  type AdminSecretState,
} from '@hasura/shared/context';
import { HttpError, UnauthorizedError, LOGIN_PATH } from '@hasura/shared/types';
import { requestJson } from '@hasura/shared/utils';
import { useAuthFetchJson } from './useFetch';

const e = React.createElement;

// Mock ONLY the network boundary (`requestJson`); everything else under test
// (header merging, error/redirect handling, callback stability) is the real
// `useAuthFetchJson` implementation.
vi.mock('@hasura/shared/utils', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/shared/utils')>()),
  requestJson: vi.fn(),
}));

const mockedRequestJson = requestJson as unknown as Mock;

function makeAuthService(
  getHeaders: () => Promise<Record<string, string>>,
): AuthService<AdminSecretState> {
  return {
    isAuthenticated: true,
    authType: 'admin-secret',
    getHeaders,
    authenticate: vi.fn(async () => true),
    initialize: vi.fn(async () => undefined),
    logout: vi.fn(),
  };
}

function PathnameProbe() {
  const { pathname } = useLocation();
  return e('span', { 'data-testid': 'pathname' }, pathname);
}

/** renderHook wrapper with a real router + a fixed AuthContext value. */
function fixedWrapper(
  auth: AuthService<AdminSecretState>,
  initialPath = '/data',
) {
  return function Wrapper({ children }: { children: React.ReactNode }) {
    return e(
      MemoryRouter,
      { initialEntries: [initialPath] },
      e(
        AppContext.Provider,
        {
          value: {
            ...defaultAppState,
            endpoints: {
              ...defaultAppState.endpoints,
              metadata: '/v1/metadata',
            },
          },
        },
        e(AuthContext.Provider, { value: auth }, children, e(PathnameProbe)),
      ),
    );
  };
}

beforeEach(() => {
  mockedRequestJson.mockReset();
  mockedRequestJson.mockResolvedValue({ ok: true });
});

describe('useAuthFetchJson — stable identity + latest committed values', () => {
  it('keeps the SAME callback identity across unrelated rerenders', () => {
    const auth = makeAuthService(vi.fn(async () => ({ h: 'v1' })));
    const { result, rerender } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth),
    });
    const first = result.current;
    rerender();
    rerender();
    expect(result.current).toBe(first);
    expect(typeof first).toBe('function');
  });

  it('keeps identity stable when the auth service (new getHeaders) is replaced, and the RETAINED callback then uses the latest committed getHeaders', async () => {
    const getHeadersV1 = vi.fn(async () => ({ token: 'v1' }));
    const getHeadersV2 = vi.fn(async () => ({ token: 'v2' }));

    const captured: Array<ReturnType<typeof useAuthFetchJson>> = [];
    function Probe() {
      captured.push(useAuthFetchJson());
      return null;
    }
    function Harness() {
      const [auth, setAuth] = React.useState(() =>
        makeAuthService(getHeadersV1),
      );
      return e(
        MemoryRouter,
        { initialEntries: ['/data'] },
        e(
          AuthContext.Provider,
          { value: auth },
          e(Probe),
          e(PathnameProbe),
          e(
            'button',
            {
              'data-testid': 'swap-auth',
              onClick: () => setAuth(makeAuthService(getHeadersV2)),
            },
            'swap',
          ),
        ),
      );
    }

    render(e(Harness));
    const firstCallback = captured[0];

    // Replace the whole auth service (brand-new getHeaders closure, as
    // useAdminSecretAuth does every render).
    await userEvent.click(screen.getByTestId('swap-auth'));

    const latestCallback = captured[captured.length - 1];
    // Identity is stable despite the replaced auth service + new getHeaders.
    expect(latestCallback).toBe(firstCallback);

    // The RETAINED (original) reference now uses the latest committed getHeaders.
    await act(async () => {
      await firstCallback('/v1/metadata');
    });
    expect(getHeadersV2).toHaveBeenCalledTimes(1);
    expect(getHeadersV1).not.toHaveBeenCalled();
    expect(mockedRequestJson).toHaveBeenCalledWith(
      '/v1/metadata',
      expect.objectContaining({
        headers: expect.objectContaining({ token: 'v2' }),
      }),
    );
  });

  it('is stable (and keeps working) under StrictMode double-invocation', async () => {
    const getHeaders = vi.fn(async () => ({ token: 'strict' }));
    const captured: Array<ReturnType<typeof useAuthFetchJson>> = [];
    function Probe() {
      captured.push(useAuthFetchJson());
      return null;
    }
    render(
      e(
        StrictMode,
        null,
        e(
          MemoryRouter,
          { initialEntries: ['/data'] },
          e(
            AuthContext.Provider,
            { value: makeAuthService(getHeaders) },
            e(Probe),
          ),
        ),
      ),
    );
    // All captured callbacks (incl. StrictMode's extra render) are identical.
    expect(new Set(captured).size).toBe(1);
    await act(async () => {
      await captured[0]('/v1/metadata');
    });
    expect(mockedRequestJson).toHaveBeenCalledWith(
      '/v1/metadata',
      expect.objectContaining({
        headers: expect.objectContaining({ token: 'strict' }),
      }),
    );
  });
});

describe('useAuthFetchJson — header precedence, generics, API', () => {
  it('merges config headers with auth headers, auth wins', async () => {
    const auth = makeAuthService(async () => ({
      shared: 'auth',
      authOnly: 'a',
    }));
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth),
    });
    await act(async () => {
      await result.current('/url', {
        method: 'POST',
        headers: { shared: 'config', configOnly: 'c' },
      });
    });
    expect(mockedRequestJson).toHaveBeenCalledWith('/url', {
      method: 'POST',
      headers: { shared: 'auth', configOnly: 'c', authOnly: 'a' },
    });
  });

  it('preserves the generic return type / value passthrough', async () => {
    type Resp = { id: number; name: string };
    const value: Resp = { id: 1, name: 'x' };
    mockedRequestJson.mockResolvedValueOnce(value);
    const auth = makeAuthService(async () => ({}));
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth),
    });
    let out: Resp | undefined;
    await act(async () => {
      out = await result.current<Resp>('/url');
    });
    expect(out).toEqual(value);
  });
});

describe('useAuthFetchJson — auth failure + 401 redirects', () => {
  it('on getHeaders rejection OFF login: redirects to LOGIN_PATH, throws UnauthorizedError, does not call requestJson', async () => {
    const auth = makeAuthService(async () => {
      throw new Error('no headers');
    });
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth, '/data'),
    });
    let caught: unknown;
    await act(async () => {
      try {
        await result.current('/url');
      } catch (err) {
        caught = err;
      }
    });
    expect(caught).toBeInstanceOf(UnauthorizedError);
    expect(mockedRequestJson).not.toHaveBeenCalled();
    expect(screen.getByTestId('pathname').textContent).toBe(LOGIN_PATH);
  });

  it('on getHeaders rejection ON login: swallows, proceeds to requestJson with no auth headers, no redirect', async () => {
    const auth = makeAuthService(async () => {
      throw new Error('no headers');
    });
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth, LOGIN_PATH),
    });
    await act(async () => {
      await result.current('/url', { headers: { c: '1' } });
    });
    expect(mockedRequestJson).toHaveBeenCalledWith('/url', {
      headers: { c: '1' },
    });
    expect(screen.getByTestId('pathname').textContent).toBe(LOGIN_PATH);
  });

  it('on 401 HttpError OFF login: redirects + throws UnauthorizedError(message)', async () => {
    mockedRequestJson.mockRejectedValueOnce(new HttpError(401, 'nope'));
    const auth = makeAuthService(async () => ({}));
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth, '/data'),
    });
    let caught: unknown;
    await act(async () => {
      try {
        await result.current('/url');
      } catch (err) {
        caught = err;
      }
    });
    expect(caught).toBeInstanceOf(UnauthorizedError);
    expect((caught as Error).message).toBe('nope');
    expect(screen.getByTestId('pathname').textContent).toBe(LOGIN_PATH);
  });

  it('on 401 HttpError ON login: rethrows the original HttpError, no redirect', async () => {
    mockedRequestJson.mockRejectedValueOnce(new HttpError(401, 'nope'));
    const auth = makeAuthService(async () => ({}));
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth, LOGIN_PATH),
    });
    let caught: unknown;
    await act(async () => {
      try {
        await result.current('/url');
      } catch (err) {
        caught = err;
      }
    });
    expect(caught).toBeInstanceOf(HttpError);
    expect(screen.getByTestId('pathname').textContent).toBe(LOGIN_PATH);
  });

  it('on a non-401 error: rethrows as-is, no redirect', async () => {
    mockedRequestJson.mockRejectedValueOnce(new HttpError(500, 'boom'));
    const auth = makeAuthService(async () => ({}));
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth, '/data'),
    });
    let caught: unknown;
    await act(async () => {
      try {
        await result.current('/url');
      } catch (err) {
        caught = err;
      }
    });
    expect(caught).toBeInstanceOf(HttpError);
    expect((caught as HttpError).status).toBe(500);
    expect(screen.getByTestId('pathname').textContent).toBe('/data');
  });
});

describe('useAuthFetchJson — per-instance isolation', () => {
  it('two hook instances with different auth services do not bleed credentials', async () => {
    const getHeadersA = vi.fn(async () => ({ token: 'A' }));
    const getHeadersB = vi.fn(async () => ({ token: 'B' }));
    const a = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(makeAuthService(getHeadersA)),
    });
    const b = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(makeAuthService(getHeadersB)),
    });
    await act(async () => {
      await a.result.current('/a');
      await b.result.current('/b');
    });
    expect(mockedRequestJson).toHaveBeenCalledWith(
      '/a',
      expect.objectContaining({ headers: { token: 'A' } }),
    );
    expect(mockedRequestJson).toHaveBeenCalledWith(
      '/b',
      expect.objectContaining({ headers: { token: 'B' } }),
    );
  });
});

describe('useAuthFetchJson — consumer effect dependency behavior', () => {
  // Mirrors how the RunQuery hooks (useDatabaseOperators / useLegacyTableColumns
  // / useTableName) now consume the fetcher: listed directly in an effect's
  // dependency array. Because the fetcher identity is stable, replacing the auth
  // service must NOT re-run the effect (no refetch loop), while changing a real
  // dependency (the "source/table/endpoint" input) MUST re-run it.
  it('does NOT re-run a consumer effect when the auth service changes, but DOES when a real dependency changes', async () => {
    const runs: string[] = [];
    function Consumer({ source }: { source: string }) {
      const fetchJson = useAuthFetchJson();
      React.useEffect(() => {
        runs.push(source);
        void fetchJson(`/v1/metadata?source=${source}`);
      }, [fetchJson, source]);
      return null;
    }
    function Harness() {
      const [auth, setAuth] = React.useState(() =>
        makeAuthService(async () => ({ token: '1' })),
      );
      const [source, setSource] = React.useState('pg_one');
      return e(
        MemoryRouter,
        { initialEntries: ['/data'] },
        e(
          AuthContext.Provider,
          { value: auth },
          e(Consumer, { source }),
          e(
            'button',
            {
              'data-testid': 'swap-auth',
              onClick: () =>
                setAuth(makeAuthService(async () => ({ token: '2' }))),
            },
            'swap',
          ),
          e(
            'button',
            {
              'data-testid': 'change-source',
              onClick: () => setSource('pg_two'),
            },
            'change',
          ),
        ),
      );
    }

    render(e(Harness));
    await waitFor(() => expect(runs).toEqual(['pg_one']));

    // Auth service replaced (new getHeaders) -> stable fetcher -> no re-run.
    await userEvent.click(screen.getByTestId('swap-auth'));
    expect(runs).toEqual(['pg_one']);

    // Real dependency changed -> effect re-runs.
    await userEvent.click(screen.getByTestId('change-source'));
    await waitFor(() => expect(runs).toEqual(['pg_one', 'pg_two']));
  });
});

describe('useAuthFetchJson — metadata authentication failures', () => {
  it.each([
    [400, 'access-denied'],
    [403, 'access-denied'],
    [400, 'invalid-jwt'],
    [400, 'jwt-invalid-claims'],
    [400, 'jwt-missing-role-claims'],
    [403, 'permission-denied'],
    [401, undefined],
  ])('expires the session for HTTP %s / %s', async (status, code) => {
    const error = new HttpError(status, 'auth rejected', { code });
    mockedRequestJson.mockRejectedValueOnce(error);
    const auth = makeAuthService(async () => ({}));
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth),
    });
    await act(async () => {
      await expect(
        result.current('/v1/metadata', { method: 'POST' }),
      ).rejects.toBeInstanceOf(UnauthorizedError);
    });
    expect(auth.logout).toHaveBeenCalledTimes(1);
    expect(screen.getByTestId('pathname').textContent).toBe(LOGIN_PATH);
  });

  it.each([
    [400, { code: 'parse-failed' }, '/v1/metadata'],
    [400, { code: 'validation-failed' }, '/v1/metadata'],
    [403, { code: 'unexpected' }, '/v1/metadata'],
    [400, 'access-denied', '/v1/metadata'],
    [400, null, '/v1/metadata'],
    [500, { code: 'access-denied' }, '/v1/metadata'],
    ['network-error', { code: 'access-denied' }, '/v1/metadata'],
    [
      403,
      { code: 'access-denied' },
      'https://another-service.example/v1/metadata',
    ],
    [400, { code: 'access-denied' }, '/v1/graphql'],
  ] as const)(
    'preserves non-session errors: %s %j %s',
    async (status, payload, url) => {
      const error = new HttpError(status, 'not an expired session', payload);
      mockedRequestJson.mockRejectedValueOnce(error);
      const auth = makeAuthService(async () => ({}));
      const { result } = renderHook(() => useAuthFetchJson(), {
        wrapper: fixedWrapper(auth),
      });
      await act(async () => {
        await expect(result.current(url)).rejects.toBe(error);
      });
      expect(auth.logout).not.toHaveBeenCalled();
      expect(screen.getByTestId('pathname').textContent).toBe('/data');
    },
  );

  it('keeps metadata login errors available to the login form', async () => {
    const error = new HttpError(400, 'invalid token', { code: 'invalid-jwt' });
    mockedRequestJson.mockRejectedValueOnce(error);
    const auth = makeAuthService(async () => ({}));
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: fixedWrapper(auth, LOGIN_PATH),
    });
    await act(async () => {
      await expect(result.current('/v1/metadata')).rejects.toBe(error);
    });
    expect(auth.logout).not.toHaveBeenCalled();
    expect(screen.getByTestId('pathname').textContent).toBe(LOGIN_PATH);
  });

  it('uses the latest committed logout when a retained fetcher fails', async () => {
    const authA = makeAuthService(async () => ({}));
    const authB = makeAuthService(async () => ({}));
    function Harness({ children }: { children: React.ReactNode }) {
      const [auth, setAuth] = React.useState(authA);
      return e(
        MemoryRouter,
        { initialEntries: ['/data'] },
        e(
          AuthContext.Provider,
          { value: auth },
          children,
          e('button', { onClick: () => setAuth(authB) }, 'change session'),
          e(PathnameProbe),
        ),
      );
    }
    const { result } = renderHook(() => useAuthFetchJson(), {
      wrapper: Harness,
    });
    const retained = result.current;
    await userEvent.click(screen.getByText('change session'));
    mockedRequestJson.mockRejectedValueOnce(new HttpError(401, 'expired'));
    await act(async () => {
      await expect(retained('/v1/metadata')).rejects.toBeInstanceOf(
        UnauthorizedError,
      );
    });
    expect(result.current).toBe(retained);
    expect(authA.logout).not.toHaveBeenCalled();
    expect(authB.logout).toHaveBeenCalledTimes(1);
  });
});
