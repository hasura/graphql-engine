import { vi } from 'vitest';
import type { EnvVars, OAuthTokenResponse } from '@hasura/shared/types';
import type { EnterpriseAuthState } from './types';
import { refreshSsoSession, resetRefreshSession } from './refreshSession';

const mocks = vi.hoisted(() => ({
  loadConsoleAuthState: vi.fn(),
  saveConsoleAuthState: vi.fn(),
  retrieveByRefreshToken: vi.fn(),
}));
vi.mock('@hasura/console-legacy-ce', () => ({
  loadConsoleAuthState: mocks.loadConsoleAuthState,
  saveConsoleAuthState: mocks.saveConsoleAuthState,
}));
vi.mock('./utils', async (importOriginal) => ({
  ...(await importOriginal<typeof import('./utils')>()),
  retrieveByRefreshToken: mocks.retrieveByRefreshToken,
  shouldRefreshAuthState: (state: { expiry?: string }) =>
    state.expiry === 'expired',
}));

const envVars = {
  consoleMode: 'server',
  consoleType: 'cloud',
  consoleId: 'hasura-client',
  hasuraOAuthUrl: 'https://oauth.example.com',
  urlPrefix: '/console',
} as unknown as EnvVars;

const makeState = (refreshToken?: string): EnterpriseAuthState => ({
  type: 'hasura-sso',
  idToken: 'old-id-token',
  accessToken: 'old-access-token',
  refreshToken,
  expiry: 'expired',
  project: null,
});

const makeJwt = (payload: Record<string, unknown>) =>
  [{ alg: 'none', typ: 'JWT' }, payload]
    .map((part) => btoa(JSON.stringify(part)).replace(/=+$/, ''))
    .join('.') + '.signature';

const tokenResponse = (refreshToken?: string) =>
  ({
    id_token: makeJwt({ sub: `user-${refreshToken}` }),
    access_token: `access-${refreshToken}`,
    refresh_token: refreshToken,
    expires_in: 3599,
  }) as OAuthTokenResponse;

const deferred = <T>() => {
  let resolve!: (value: T) => void;
  let reject!: (reason: unknown) => void;
  const promise = new Promise<T>((res, rej) => {
    resolve = res;
    reject = rej;
  });

  return { promise, resolve, reject };
};

beforeEach(() => {
  resetRefreshSession();
  mocks.loadConsoleAuthState.mockReturnValue(null);
});
afterEach(() => {
  vi.resetAllMocks();
});

describe('refreshSsoSession', () => {
  it('sends a single refresh request for concurrent callers', async () => {
    const response = deferred<OAuthTokenResponse>();
    mocks.retrieveByRefreshToken.mockReturnValue(response.promise);

    const results = Promise.all([
      refreshSsoSession(makeState('rt-1'), envVars),
      refreshSsoSession(makeState('rt-1'), envVars),
      refreshSsoSession(makeState('rt-1'), envVars),
    ]);
    response.resolve(tokenResponse('rt-2'));

    const [first, ...rest] = await results;
    expect(mocks.retrieveByRefreshToken).toHaveBeenCalledOnce();
    expect(first).toMatchObject({ refreshToken: 'rt-2' });
    rest.forEach((state) => expect(state).toBe(first));
    expect(mocks.saveConsoleAuthState).toHaveBeenCalledOnce();
    expect(mocks.saveConsoleAuthState).toHaveBeenCalledWith(first);
  });

  it('reuses the result for a caller still holding the rotated refresh token', async () => {
    mocks.retrieveByRefreshToken.mockResolvedValue(tokenResponse('rt-2'));

    const first = await refreshSsoSession(makeState('rt-1'), envVars);
    const second = await refreshSsoSession(makeState('rt-1'), envVars);

    expect(mocks.retrieveByRefreshToken).toHaveBeenCalledOnce();
    expect(second).toBe(first);
  });

  it('refreshes again with a refresh token the provider did not rotate', async () => {
    mocks.retrieveByRefreshToken.mockResolvedValue(tokenResponse());

    const first = await refreshSsoSession(makeState('rt-1'), envVars);
    await refreshSsoSession(makeState('rt-1'), envVars);

    expect(first).toMatchObject({ refreshToken: 'rt-1' });
    expect(mocks.retrieveByRefreshToken).toHaveBeenCalledTimes(2);
  });

  it('retries after a failed refresh', async () => {
    mocks.retrieveByRefreshToken
      .mockRejectedValueOnce(new Error('network error'))
      .mockResolvedValueOnce(tokenResponse('rt-2'));

    await expect(refreshSsoSession(makeState('rt-1'), envVars)).rejects.toThrow(
      'network error',
    );
    await expect(
      refreshSsoSession(makeState('rt-1'), envVars),
    ).resolves.toMatchObject({ refreshToken: 'rt-2' });
    expect(mocks.retrieveByRefreshToken).toHaveBeenCalledTimes(2);
  });

  it('adopts a session another tab already refreshed', async () => {
    const saved = { ...makeState('rt-2'), expiry: 'fresh' };
    mocks.loadConsoleAuthState.mockReturnValue(saved);

    await expect(refreshSsoSession(makeState('rt-1'), envVars)).resolves.toBe(
      saved,
    );
    expect(mocks.retrieveByRefreshToken).not.toHaveBeenCalled();
  });

  it.each([
    ['the same refresh token', { ...makeState('rt-1'), expiry: 'fresh' }],
    ['an expired session', makeState('rt-2')],
    [
      'a different session type',
      {
        type: 'sso',
        clientId: 'other',
        idToken: 'id',
        accessToken: 'access',
        refreshToken: 'rt-2',
        expiry: 'fresh',
      },
    ],
  ])('does not adopt a saved session with %s', async (_, saved) => {
    mocks.loadConsoleAuthState.mockReturnValue(saved);
    mocks.retrieveByRefreshToken.mockResolvedValue(tokenResponse('rt-3'));

    await expect(
      refreshSsoSession(makeState('rt-1'), envVars),
    ).resolves.toMatchObject({ refreshToken: 'rt-3' });
    expect(mocks.retrieveByRefreshToken).toHaveBeenCalledOnce();
  });

  it('neither reads nor saves the session in CLI mode', async () => {
    mocks.retrieveByRefreshToken.mockResolvedValue(tokenResponse('rt-2'));

    await refreshSsoSession(makeState('rt-1'), {
      ...envVars,
      consoleMode: 'cli',
    } as EnvVars);

    expect(mocks.loadConsoleAuthState).not.toHaveBeenCalled();
    expect(mocks.saveConsoleAuthState).not.toHaveBeenCalled();
  });

  it('rejects a session without a refresh token', async () => {
    await expect(refreshSsoSession(makeState(), envVars)).rejects.toThrow(
      'no refresh token',
    );
    expect(mocks.retrieveByRefreshToken).not.toHaveBeenCalled();
  });
});
