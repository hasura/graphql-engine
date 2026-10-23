import { vi } from 'vitest';
import type { EnvVars } from '@hasura/shared/types';
import {
  getCurrentSsoIdentityProvider,
  getHasuraSsoIdentityProvider,
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
