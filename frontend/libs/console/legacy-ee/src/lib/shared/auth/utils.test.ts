import { vi } from 'vitest';
import type { EnvVars } from '@hasura/shared/types';
import type { EnterpriseAuthState } from './types';
import {
  getAuthStateExpiry,
  getCurrentSsoIdentityProvider,
  getHasuraSsoIdentityProvider,
  shouldRefreshAuthState,
} from './utils';

const mocks = vi.hoisted(() => ({ clientId: 'other-client' }));
vi.mock('./localStorage', async (importOriginal) => {
  const actual = await importOriginal<typeof import('./localStorage')>();
  return {
    ...actual,
    getKeyFromLS: vi.fn((key) =>
      key === 'client_id' ? mocks.clientId : undefined,
    ),
    modifyKey: vi.fn(),
  };
});

const envVars = {
  consoleMode: 'server',
  consoleType: 'cloud',
  consoleId: 'hasura-client',
  hasuraOAuthUrl: 'https://oauth.example.com',
  ssoIdentityProviders: [
    {
      client_id: 'other-client',
      name: 'Other',
      scope: 'openid',
      authorization_url: 'https://other.example.com/auth',
      request_token_url: 'https://other.example.com/token',
    },
  ],
} as unknown as EnvVars;

describe('Cloud OAuth callback provider', () => {
  it('ignores a persisted third-party provider when completing Cloud OAuth', () => {
    expect(getCurrentSsoIdentityProvider(envVars)?.client_id).toBe(
      'hasura-client',
    );
  });

  it('still resolves the persisted provider for self-hosted SSO', () => {
    expect(
      getCurrentSsoIdentityProvider({
        consoleMode: 'server',
        consoleType: 'pro',
        consoleId: 'hasura-client',
        serverVersion: 'v2.51.1',
        urlPrefix: '/console',
        assetsPath: '',
        consolePath: '/console',
        enableTelemetry: false,
        isAdminSecretSet: true,
        isAdminSecretDisabled: false,
        schemaRegistryHost: '',
        ssoIdentityProviders: [
          {
            client_id: 'other-client',
            name: 'Other',
            scope: 'openid',
            authorization_url: 'https://other.example.com/auth',
            request_token_url: 'https://other.example.com/token',
          },
        ],
      })?.client_id,
    ).toBe('other-client');
  });
});

describe('Hasura SSO identity provider', () => {
  it('does not duplicate slashes when the OAuth URL has a trailing slash', () => {
    const provider = getHasuraSsoIdentityProvider({
      ...envVars,
      hasuraOAuthUrl: 'https://oauth.example.com/',
    } as EnvVars);

    expect(provider?.authorization_url).toBe(
      'https://oauth.example.com/oauth2/auth',
    );
    expect(provider?.request_token_url).toBe(
      'https://oauth.example.com/oauth2/token',
    );
  });
});

describe('SSO token expiry', () => {
  const now = Date.UTC(2026, 0, 1);
  const makeJwt = (payload: Record<string, unknown>) =>
    [{ alg: 'none', typ: 'JWT' }, payload]
      .map((part) => btoa(JSON.stringify(part)).replace(/=+$/, ''))
      .join('.') + '.signature';
  const makeState = (
    idToken: string,
    expiry?: number,
  ): EnterpriseAuthState => ({
    type: 'hasura-sso',
    idToken,
    accessToken: 'access-token',
    refreshToken: 'refresh-token',
    expiry: expiry?.toString(),
    project: null,
  });

  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(now);
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it('uses the id token exp claim when it expires before expires_in', () => {
    const idTokenExpiry = now + 10 * 60 * 1000;
    const state = makeState(
      makeJwt({ exp: idTokenExpiry / 1000 }),
      now + 60 * 60 * 1000,
    );

    expect(getAuthStateExpiry(state)).toBe(idTokenExpiry);
    expect(shouldRefreshAuthState(state)).toBe(false);

    vi.setSystemTime(idTokenExpiry - 30 * 1000);
    expect(shouldRefreshAuthState(state)).toBe(true);
  });

  it('uses expires_in when it expires before the id token', () => {
    const expiry = now + 10 * 60 * 1000;
    const state = makeState(
      makeJwt({ exp: (now + 60 * 60 * 1000) / 1000 }),
      expiry,
    );

    expect(getAuthStateExpiry(state)).toBe(expiry);
  });

  it('falls back to the id token exp claim without expires_in', () => {
    const state = makeState(makeJwt({ exp: (now - 1000) / 1000 }));

    expect(shouldRefreshAuthState(state)).toBe(true);
  });

  it('falls back to expires_in for opaque tokens', () => {
    const expiry = now + 10 * 60 * 1000;

    expect(getAuthStateExpiry(makeState('opaque-token', expiry))).toBe(expiry);
  });

  it('never refreshes without any known expiry', () => {
    expect(shouldRefreshAuthState(makeState('opaque-token'))).toBe(false);
    expect(shouldRefreshAuthState(makeState(makeJwt({ sub: 'user' })))).toBe(
      false,
    );
  });
});
