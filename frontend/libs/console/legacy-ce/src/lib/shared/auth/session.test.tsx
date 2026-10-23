import { act, renderHook, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, useLocation } from 'react-router';
import { AppContext, defaultAppState } from '@hasura/shared/context';
import { HttpError, LS_KEYS } from '@hasura/shared/types';
import { requestJson } from '@hasura/shared/utils';
import { useAdminSecretAuth } from './useAdminSecretAuth';
import { useNoAuth } from './useNoAuth';

vi.mock('@hasura/shared/utils', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/shared/utils')>()),
  requestJson: vi.fn(),
}));

function Pathname() {
  return <span data-testid="pathname">{useLocation().pathname}</span>;
}

afterEach(() => {
  localStorage.clear();
  vi.resetAllMocks();
});

function mount(useService: typeof useNoAuth, withSecret = false) {
  const client = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
  const { result } = renderHook(
    () => {
      const auth = useService();
      return auth;
    },
    {
      wrapper: ({ children }) => (
        <QueryClientProvider client={client}>
          <AppContext.Provider
            value={{
              ...defaultAppState,
              endpoints: {
                ...defaultAppState.endpoints,
                metadata: '/v1/metadata',
              },
              envVars: {
                ...defaultAppState.envVars,
                consoleMode: 'server',
                consoleType: 'oss',
                cdnAssets: false,
                assetsPath: '',
                consolePath: '',
                enableTelemetry: false,
                isAdminSecretDisabled: false,
                isAdminSecretSet: withSecret,
              },
            }}
          >
            <MemoryRouter initialEntries={['/data']}>
              {children}
              <Pathname />
            </MemoryRouter>
          </AppContext.Provider>
        </QueryClientProvider>
      ),
    },
  );
  return { result, client };
}

describe('CE credential validation and logout', () => {
  it('rejects a saved admin secret despite fresh metadata in the query cache', async () => {
    localStorage.setItem(
      LS_KEYS.consoleAuthState,
      JSON.stringify({
        type: 'admin-secret',
        adminSecret: 'revoked-secret',
      }),
    );
    const { result, client } = mount(useAdminSecretAuth, true);
    client.setQueryData(['INCONSISTENT_METADATA'], {
      is_consistent: true,
      inconsistent_objects: [],
    });
    vi.mocked(requestJson).mockRejectedValueOnce(new HttpError(401, 'revoked'));
    await act(() => result.current.initialize());
    expect(requestJson).toHaveBeenCalledTimes(1);
    expect(result.current.isAuthenticated).toBe(false);
    expect(screen.getByTestId('pathname')).toHaveTextContent('/login');
  });

  it('rejects no-auth initialization despite a fresh cached success', async () => {
    const { result, client } = mount(useNoAuth);
    client.setQueryData(['INCONSISTENT_METADATA'], {
      is_consistent: true,
      inconsistent_objects: [],
    });
    vi.mocked(requestJson).mockRejectedValueOnce(
      new HttpError(400, 'not admin', { code: 'access-denied' }),
    );
    await act(() => result.current.initialize());
    expect(requestJson).toHaveBeenCalledTimes(1);
    expect(result.current.isAuthenticated).toBe(false);
    expect(screen.getByTestId('pathname')).toHaveTextContent('/login');
  });

  it.each([
    ['no-auth', useNoAuth, undefined],
    [
      'admin-secret',
      useAdminSecretAuth,
      {
        type: 'admin-secret',
        adminSecret: 'secret',
        shouldPersist: true,
      },
    ],
  ] as const)(
    'clears %s session state and cached data on logout',
    async (_, service, input) => {
      const { result, client } = mount(service);
      vi.mocked(requestJson).mockResolvedValueOnce({
        is_consistent: true,
        inconsistent_objects: [],
      });
      await act(() => result.current.authenticate(input as any));
      expect(result.current.isAuthenticated).toBe(true);
      client.setQueryData(['sensitive-metadata'], { source: 'private' });
      await act(() => result.current.logout());
      expect(result.current.isAuthenticated).toBe(false);
      expect(client.getQueryCache().getAll()).toHaveLength(0);
      expect(localStorage.getItem(LS_KEYS.consoleAuthState)).toBeNull();
      expect(await result.current.getHeaders()).toEqual({});
    },
  );
});
