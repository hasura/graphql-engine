import { renderHook, act, cleanup } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { vi } from 'vitest';
import useEnterpriseAuth from './useEnterpriseAuth';
import type { EnterpriseAuthState } from './types';

const mocks = vi.hoisted(() => ({
  loadConsoleAuthState: vi.fn(),
  clearConsoleAuthState: vi.fn(),
  saveConsoleAuthState: vi.fn(),
  saveAdminSecretState: vi.fn(),
  fetchMetadata: vi.fn(),
  runGraphQL: vi.fn(),
  navigateAuth: vi.fn(),
  clearQueries: vi.fn(),
  envVars: {
    consoleType: 'cloud',
    consoleMode: 'server',
    adminSecret: '' as string | undefined,
  },
  globals: { isAdminSecretSet: true, adminSecret: 'global-secret' },
}));
vi.mock('@hasura/console-legacy-ce', () => ({
  globals: mocks.globals,
  loadConsoleAuthState: mocks.loadConsoleAuthState,
  clearConsoleAuthState: mocks.clearConsoleAuthState,
  saveConsoleAuthState: mocks.saveConsoleAuthState,
  saveAdminSecretState: mocks.saveAdminSecretState,
  hasDataAccess: (privileges?: string[]) =>
    !privileges?.length || privileges.includes('admin'),
  hasGraphQLAccess: (privileges?: string[]) =>
    !privileges?.length ||
    privileges.includes('admin') ||
    privileges.includes('graphql_admin'),
}));
vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({
    envVars: mocks.envVars,
    endpoints: { graphQLUrl: 'http://engine/v1/graphql' },
  }),
}));
vi.mock('@tanstack/react-query', () => ({
  useQueryClient: () => ({ clear: mocks.clearQueries }),
}));
vi.mock('@hasura/metadata/api', () => ({
  useFetchInconsistentMetadata: () => mocks.fetchMetadata,
  runGraphQL: mocks.runGraphQL,
}));
vi.mock('./useNavigateAuth', () => ({ default: () => mocks.navigateAuth }));
vi.mock('./utils', async (importOriginal) => ({
  ...(await importOriginal<typeof import('./utils')>()),
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
  mocks.envVars.consoleMode = 'server';
  mocks.envVars.adminSecret = '';
  mocks.globals.adminSecret = 'global-secret';
  mocks.globals.isAdminSecretSet = true;
  mocks.loadConsoleAuthState.mockReturnValue(null);
  mocks.fetchMetadata.mockResolvedValue({});
  mocks.runGraphQL.mockResolvedValue({});
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
    cloudState,
  ])('prefers the injected admin secret over saved state %j', async (saved) => {
    mocks.envVars.adminSecret = 'injected-cloud-secret';
    mocks.loadConsoleAuthState.mockReturnValue(saved);
    const { result } = mount();

    await act(() => result.current.initialize());

    expect(mocks.fetchMetadata).toHaveBeenCalledWith(
      expect.objectContaining({
        'x-hasura-admin-secret': 'injected-cloud-secret',
      }),
    );
    expect(await result.current.getHeaders()).toEqual(
      expect.objectContaining({
        'x-hasura-admin-secret': 'injected-cloud-secret',
      }),
    );
    expect(mocks.navigateAuth).toHaveBeenCalledWith({
      type: 'admin-secret',
      adminSecret: 'injected-cloud-secret',
      shouldPersist: false,
    });
    expect(result.current.isAuthenticated).toBe(true);
    expect(mocks.loadConsoleAuthState).not.toHaveBeenCalled();
    expect(mocks.clearConsoleAuthState).not.toHaveBeenCalled();
    expect(mocks.saveAdminSecretState).not.toHaveBeenCalled();
    expect(mocks.saveConsoleAuthState).not.toHaveBeenCalled();
  });

  it('rejects an invalid injected secret instead of restoring a saved session', async () => {
    mocks.envVars.adminSecret = 'invalid-cloud-secret';
    mocks.loadConsoleAuthState.mockReturnValue(cloudState);
    mocks.fetchMetadata.mockRejectedValue(new Error('unauthorized'));
    const { result } = mount();

    await act(() => result.current.initialize());

    expect(mocks.fetchMetadata).toHaveBeenCalledWith(
      expect.objectContaining({
        'x-hasura-admin-secret': 'invalid-cloud-secret',
      }),
    );
    expect(mocks.clearConsoleAuthState).toHaveBeenCalledOnce();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(null);
    expect(result.current.isAuthenticated).toBe(false);
    expect(await result.current.getHeaders()).not.toHaveProperty(
      'x-hasura-admin-secret',
    );
    expect(mocks.saveAdminSecretState).not.toHaveBeenCalled();
    expect(mocks.saveConsoleAuthState).not.toHaveBeenCalled();
  });

  it('requires Hasura OAuth when the injected secret is absent', async () => {
    mocks.envVars.adminSecret = undefined;
    const { result } = mount();

    await act(() => result.current.initialize());

    expect(mocks.navigateAuth).toHaveBeenCalledWith(null);
    expect(mocks.fetchMetadata).not.toHaveBeenCalled();
    expect(result.current.isAuthenticated).toBe(false);
  });

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
    expect(result.current.idToken).toBe(cloudState.idToken);
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

  it('validates a GraphQL-only collaborator without the metadata API', async () => {
    const saved: EnterpriseAuthState = {
      ...cloudState,
      project: { privileges: ['graphql_admin'] },
    };
    mocks.loadConsoleAuthState.mockReturnValue(saved);
    const { result } = mount();
    await act(() => result.current.initialize());
    expect(mocks.fetchMetadata).not.toHaveBeenCalled();
    expect(mocks.runGraphQL).toHaveBeenCalledWith(
      expect.objectContaining({
        url: 'http://engine/v1/graphql',
        headers: expect.objectContaining({
          'hasura-collaborator-token': `IDToken ${cloudState.idToken}`,
        }),
      }),
    );
    expect(mocks.navigateAuth).toHaveBeenCalledWith(saved);
    expect(result.current.privileges).toEqual(['graphql_admin']);
  });

  it('clears a GraphQL-only session rejected by the engine', async () => {
    mocks.loadConsoleAuthState.mockReturnValue({
      ...cloudState,
      project: { privileges: ['graphql_admin'] },
    });
    mocks.runGraphQL.mockRejectedValue(new Error('invalid token'));
    const { result } = mount();
    await act(() => result.current.initialize());
    expect(mocks.clearConsoleAuthState).toHaveBeenCalledOnce();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(null);
    expect(result.current.isAuthenticated).toBe(false);
  });

  it('accepts a metrics-only collaborator without calling the engine', async () => {
    const saved: EnterpriseAuthState = {
      ...cloudState,
      project: { privileges: ['view_metrics'] },
    };
    mocks.loadConsoleAuthState.mockReturnValue(saved);
    const { result } = mount();
    await act(() => result.current.initialize());
    expect(mocks.fetchMetadata).not.toHaveBeenCalled();
    expect(mocks.runGraphQL).not.toHaveBeenCalled();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(saved);
    expect(result.current.isAuthenticated).toBe(true);
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
    expect(result.current.idToken).toBeUndefined();
    expect(mocks.clearConsoleAuthState).not.toHaveBeenCalled();
  });
});

describe('Self-hosted session initialization', () => {
  beforeEach(() => {
    mocks.envVars.consoleType = 'pro-lite';
  });

  it('uses the CLI-injected secret even when isAdminSecretSet is true', async () => {
    mocks.envVars.consoleMode = 'cli';
    mocks.envVars.adminSecret = 'cli-secret';
    mocks.loadConsoleAuthState.mockReturnValue({
      type: 'pat',
      pat: 'old-pat',
    });
    const { result } = mount();

    await act(() => result.current.initialize());

    expect(mocks.fetchMetadata).toHaveBeenCalledWith(
      expect.objectContaining({ 'x-hasura-admin-secret': 'cli-secret' }),
    );
    expect(await result.current.getHeaders()).toEqual(
      expect.objectContaining({ 'x-hasura-admin-secret': 'cli-secret' }),
    );
    expect(mocks.navigateAuth).toHaveBeenCalledWith({
      type: 'admin-secret',
      adminSecret: 'cli-secret',
      shouldPersist: false,
    });
    expect(result.current.isAuthenticated).toBe(true);
    expect(mocks.saveAdminSecretState).not.toHaveBeenCalled();
    expect(mocks.saveConsoleAuthState).not.toHaveBeenCalled();
  });

  it('requires login when the engine needs a secret but none is supplied', async () => {
    mocks.globals.adminSecret = '';
    const { result } = mount();

    await act(() => result.current.initialize());

    expect(mocks.navigateAuth).toHaveBeenCalledWith(null);
    expect(mocks.fetchMetadata).not.toHaveBeenCalled();
    expect(result.current.isAuthenticated).toBe(false);
  });

  it('validates a no-auth engine without inventing a secret', async () => {
    mocks.globals.adminSecret = '';
    mocks.globals.isAdminSecretSet = false;
    const { result } = mount();

    await act(() => result.current.initialize());

    expect(mocks.fetchMetadata).toHaveBeenCalledOnce();
    expect(await result.current.getHeaders()).not.toHaveProperty(
      'x-hasura-admin-secret',
    );
    expect(mocks.navigateAuth).toHaveBeenCalledWith({ type: 'none' });
    expect(result.current.isAuthenticated).toBe(true);
  });

  it.each([
    { type: 'pat', pat: 'saved-pat' },
    {
      type: 'sso',
      clientId: 'other',
      idToken: 'saved-id',
      accessToken: 'saved-access',
    },
  ] as EnterpriseAuthState[])(
    'preserves a saved $type session',
    async (saved) => {
      mocks.loadConsoleAuthState.mockReturnValue(saved);
      const { result } = mount();

      await act(() => result.current.initialize());

      expect(mocks.fetchMetadata).toHaveBeenCalledOnce();
      expect(mocks.navigateAuth).toHaveBeenCalledWith(saved);
      expect(result.current.authType).toBe(saved.type);
      expect(await result.current.getHeaders()).not.toHaveProperty(
        'x-hasura-admin-secret',
      );
    },
  );

  it('rejects an invalid CLI secret without retaining authentication', async () => {
    mocks.envVars.consoleMode = 'cli';
    mocks.fetchMetadata.mockRejectedValue(new Error('unauthorized'));
    const { result } = mount();

    await act(() => result.current.initialize());

    expect(mocks.fetchMetadata).toHaveBeenCalledWith(
      expect.objectContaining({ 'x-hasura-admin-secret': 'global-secret' }),
    );
    expect(mocks.clearConsoleAuthState).toHaveBeenCalledOnce();
    expect(mocks.navigateAuth).toHaveBeenCalledWith(null);
    expect(result.current.isAuthenticated).toBe(false);
    expect(await result.current.getHeaders()).not.toHaveProperty(
      'x-hasura-admin-secret',
    );
  });
});
