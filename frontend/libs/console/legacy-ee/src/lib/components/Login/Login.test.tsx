import { render, screen, cleanup } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { vi } from 'vitest';
import Login from './Login';

const mocks = vi.hoisted(() => ({
  envVars: { consoleType: 'cloud', urlPrefix: '/console' },
  getHasuraSsoIdentityProvider: vi.fn(),
  getCurrentSsoIdentityProvider: vi.fn(),
  getOAuthAuthorizeUrl: vi.fn(() => 'https://oauth.example/oauth2/auth'),
  initiateGeneralOAuthRequest: vi.fn(),
  clearConsoleAuthState: vi.fn(),
}));
vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({ envVars: mocks.envVars }),
}));
vi.mock('../../shared/auth/utils', () => ({
  ...mocks,
  getOAuthRedirectUrl: () => 'https://cloud.example/console/oauth2/callback',
}));
vi.mock('@hasura/shared/ui', () => ({
  LoadingScreen: ({ children }: { children: React.ReactNode }) => (
    <>{children}</>
  ),
  LoadingScreenError: ({ title }: { title: string }) => <div>{title}</div>,
}));
vi.mock('@hasura/console-legacy-ce', () => ({
  clearConsoleAuthState: mocks.clearConsoleAuthState,
  LoginContainer: ({ children }: { children: React.ReactNode }) => (
    <>{children}</>
  ),
}));
vi.mock('./LoginEE', () => ({
  default: () => <div>Choose a login method</div>,
}));

const hasura = {
  client_id: 'hasura',
  authorization_url: 'https://oauth.example/oauth2/auth',
  scope: 'openid',
};
const thirdParty = { ...hasura, client_id: 'third-party' };

beforeEach(() => {
  mocks.envVars.consoleType = 'cloud';
  mocks.getHasuraSsoIdentityProvider.mockReturnValue(hasura);
  mocks.getCurrentSsoIdentityProvider.mockReturnValue(thirdParty);
});
afterEach(() => {
  cleanup();
  vi.clearAllMocks();
});

const mount = (path = '/login') =>
  render(
    <MemoryRouter initialEntries={[path]}>
      <Login />
    </MemoryRouter>,
  );

describe('Cloud login', () => {
  it('automatically starts Hasura OAuth without auto_login=true', () => {
    mount('/login?redirect_url=%2Fdata%3Fsource%3Ddefault');
    expect(mocks.getCurrentSsoIdentityProvider).not.toHaveBeenCalled();
    expect(mocks.clearConsoleAuthState).toHaveBeenCalledOnce();
    expect(mocks.getOAuthAuthorizeUrl).toHaveBeenCalledWith(
      hasura.authorization_url,
      hasura.client_id,
      hasura.scope,
      'https://cloud.example/console/oauth2/callback',
    );
    expect(mocks.initiateGeneralOAuthRequest).toHaveBeenCalledWith(
      'https://oauth.example/oauth2/auth',
      expect.objectContaining({
        pathname: '/login',
        search: '?redirect_url=%2Fdata%3Fsource%3Ddefault',
      }),
      false,
    );
    expect(screen.queryByText('Choose a login method')).toBeNull();
  });

  it('does not offer admin-secret login when Cloud OAuth is unconfigured', () => {
    mocks.getHasuraSsoIdentityProvider.mockReturnValue(null);
    mount();
    expect(screen.getByText('Hasura OAuth is not configured')).toBeDefined();
    expect(screen.queryByText('Choose a login method')).toBeNull();
  });

  it('keeps the self-hosted login picker', () => {
    mocks.envVars.consoleType = 'pro';
    mount();
    expect(screen.getByText('Choose a login method')).toBeDefined();
    expect(mocks.initiateGeneralOAuthRequest).not.toHaveBeenCalled();
  });

  it('retains explicit self-hosted auto-login and its redirect target', () => {
    mocks.envVars.consoleType = 'pro';
    mount('/login?auto_login=true&redirect_url=%2Fdata');
    expect(mocks.getOAuthAuthorizeUrl).toHaveBeenCalledWith(
      thirdParty.authorization_url,
      thirdParty.client_id,
      thirdParty.scope,
      'https://cloud.example/console/oauth2/callback',
    );
    expect(mocks.initiateGeneralOAuthRequest).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        search: '?auto_login=true&redirect_url=%2Fdata',
      }),
      false,
    );
  });
});
