import { render, screen, fireEvent, cleanup } from '@testing-library/react';
import { BrowserRouter, useLocation } from 'react-router';
import { vi } from 'vitest';
import useNavigateAuth from './useNavigateAuth';
import type { EnterpriseAuthState } from './types';

const prefix = '/project/1685116d-7f77-4d37-8efe-6cd28de0b010/console';
const mocks = vi.hoisted(() => ({
  getKeyFromLS: vi.fn(),
  modifyRedirectUrl: vi.fn(),
}));
vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({
    envVars: {
      urlPrefix: '/project/1685116d-7f77-4d37-8efe-6cd28de0b010/console',
    },
  }),
}));
vi.mock('./localStorage', () => ({ getKeyFromLS: mocks.getKeyFromLS }));
vi.mock('./utils', () => ({ modifyRedirectUrl: mocks.modifyRedirectUrl }));

const state: EnterpriseAuthState = {
  type: 'hasura-sso',
  idToken: 'test',
  accessToken: 'test',
  project: null,
};

const Harness = ({ authenticated }: { authenticated: boolean }) => {
  const navigateAuth = useNavigateAuth();
  const location = useLocation();
  return (
    <>
      <button onClick={() => navigateAuth(authenticated ? state : null)}>
        Continue
      </button>
      <div data-testid="path">
        {location.pathname}
        {location.search}
        {location.hash}
      </div>
    </>
  );
};

const mount = (path: string, authenticated = true) => {
  window.history.replaceState({}, '', prefix + path);
  render(
    <BrowserRouter basename={prefix}>
      <Harness authenticated={authenticated} />
    </BrowserRouter>,
  );
  fireEvent.click(screen.getByText('Continue'));
};

afterEach(() => {
  cleanup();
  vi.clearAllMocks();
});

describe('useNavigateAuth with a cloud BrowserRouter basename', () => {
  it('returns from login to the root without duplicating the project path', () => {
    mount('/login');
    expect(window.location.pathname).toBe(prefix);
  });

  it('returns to a query redirect without double-decoding it', () => {
    const target = '/data?name=hello%20world#table';
    mount('/login?redirect_url=' + encodeURIComponent(target));
    expect(screen.getByTestId('path').textContent).toBe(target);
    expect(window.location.pathname).toBe(prefix + '/data');
  });

  it('restores a deep link from stored OAuth state on callback', () => {
    mocks.getKeyFromLS.mockReturnValue('/data?source=default#table');
    mount('/oauth2/callback?code=test&state=test');
    expect(window.location.pathname).toBe(prefix + '/data');
    expect(screen.getByTestId('path').textContent).toBe(
      '/data?source=default#table',
    );
    expect(mocks.modifyRedirectUrl).toHaveBeenCalledWith('/');
  });

  it('also supports a stored redirect that already includes the basename', () => {
    mocks.getKeyFromLS.mockReturnValue(prefix + '/api/api-explorer');
    mount('/oauth2/callback?code=test&state=test');
    expect(window.location.pathname).toBe(prefix + '/api/api-explorer');
  });

  it('preserves the path, search, and hash before login', () => {
    mount('/data?source=default#table', false);
    expect(window.location.pathname).toBe(prefix + '/login');
    expect(
      new URLSearchParams(window.location.search).get('redirect_url'),
    ).toBe('/data?source=default#table');
  });

  it('leaves the OAuth callback alone until its token is validated', () => {
    mount('/oauth2/callback?code=test&state=test', false);
    expect(window.location.pathname).toBe(prefix + '/oauth2/callback');
  });
});
