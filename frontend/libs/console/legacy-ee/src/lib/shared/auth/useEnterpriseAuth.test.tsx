import { renderHook, act, cleanup } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { vi } from 'vitest';
import useEnterpriseAuth from './useEnterpriseAuth';
import type { EnterpriseAuthState } from './types';

const mocks = vi.hoisted(() => ({
  loadConsoleAuthState: vi.fn(),
  clearConsoleAuthState: vi.fn(),
  saveConsoleAuthState: vi.fn(),
  fetchMetadata: vi.fn(),
  navigateAuth: vi.fn(),
  clearQueries: vi.fn(),
  envVars: { consoleType: 'cloud', consoleMode: 'server' },
  globals: { isAdminSecretSet: true, adminSecret: 'global-secret' },
}));
vi.mock('@hasura/console-legacy-ce', () => ({
  globals: mocks.globals,
  loadConsoleAuthState: mocks.loadConsoleAuthState,
  clearConsoleAuthState: mocks.clearConsoleAuthState,
  saveConsoleAuthState: mocks.saveConsoleAuthState,
  saveAdminSecretState: vi.fn(),
}));
vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({ envVars: mocks.envVars }),
}));
vi.mock('@tanstack/react-query', () => ({
  useQueryClient: () => ({ clear: mocks.clearQueries }),
}));
vi.mock('@hasura/metadata/api', () => ({
  useFetchInconsistentMetadata: () => mocks.fetchMetadata,
}));
vi.mock('./useNavigateAuth', () => ({ default: () => mocks.navigateAuth }));
vi.mock('./utils', () => ({
  buildEnterpriseAuthHeaders: () => ({}),
  buildMetricsAuthHeaders: () => ({}),
  shouldRefreshAuthState: () => false,
}));

const cloudState: EnterpriseAuthState = {
  type: 'hasura-sso',
  accessToken: 'test',
  idToken: 'test',
  project: { privileges: ['admin'] },
};

beforeEach(() => {
  mocks.envVars.consoleType = 'cloud';
  mocks.loadConsoleAuthState.mockReturnValue(null);
  mocks.fetchMetadata.mockResolvedValue({});
});
afterEach(() => {
  cleanup();
  vi.resetAllMocks();
});

const mount = () =>
  renderHook(() => useEnterpriseAuth(), {
    wrapper: ({ children }) => <MemoryRouter>{children}</MemoryRouter>,
  });

describe('Cloud session initialization', () => {
  it.each([
    null,
    { type: 'none' },
    { type: 'admin-secret', adminSecret: 'old', shouldPersist: true },
    { type: 'pat', pat: 'old' },
    { type: 'sso', clientId: 'other', idToken: 'old', accessToken: 'old' },
  ])('requires Hasura OAuth instead of restoring %j', async (saved) => {
    mocks.loadConsoleAuthState.mockReturnValue(saved);
    const { result } = mount();
    await act(() => result.current.initialize());
    expect(mocks.clearConsoleAuthState).toHaveBeenCalledOnce();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(null);
    expect(mocks.fetchMetadata).not.toHaveBeenCalled();
    expect(result.current.isAuthenticated).toBe(false);
  });

  it('restores a valid Hasura session without reauthorizing', async () => {
    mocks.loadConsoleAuthState.mockReturnValue(cloudState);
    const { result } = mount();
    await act(() => result.current.initialize());
    expect(mocks.fetchMetadata).toHaveBeenCalledOnce();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(cloudState);
    expect(result.current.authType).toBe('hasura-sso');
  });

  it('clears a rejected Hasura session and sends the user back to OAuth', async () => {
    mocks.loadConsoleAuthState.mockReturnValue(cloudState);
    mocks.fetchMetadata.mockRejectedValue(new Error('unauthorized'));
    const { result } = mount();
    await act(() => result.current.initialize());
    expect(mocks.clearConsoleAuthState).toHaveBeenCalledOnce();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(null);
    expect(result.current.isAuthenticated).toBe(false);
  });

  it('still restores admin-secret auth for self-hosted consoles', async () => {
    mocks.envVars.consoleType = 'pro';
    const saved = {
      type: 'admin-secret',
      adminSecret: 'secret',
      shouldPersist: false,
    };
    mocks.loadConsoleAuthState.mockReturnValue(saved);
    const { result } = mount();
    await act(() => result.current.initialize());
    expect(mocks.fetchMetadata).toHaveBeenCalledOnce();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(saved);
    expect(mocks.clearConsoleAuthState).not.toHaveBeenCalled();
  });
});
