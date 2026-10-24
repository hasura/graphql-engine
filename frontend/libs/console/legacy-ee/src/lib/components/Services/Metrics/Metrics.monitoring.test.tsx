import React from 'react';
import {
  act,
  render,
  screen,
  waitFor,
  renderHook,
} from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { vi } from 'vitest';
import Metrics from './Metrics';
import { useProjectInfo } from '../../../hooks/useProjectInfo';
import { getHasuraMetricsUrl } from '../../../Globals';

vi.hoisted(() => {
  window.__env = {} as typeof window.__env;
});

const PROJECT = '11111111-1111-4111-8111-111111111111';
const OTHER = '22222222-2222-4222-8222-222222222222';
const state = vi.hoisted(() => ({
  envVars: {} as Record<string, any>,
  auth: {} as Record<string, any>,
  makeApolloClient: vi.fn(() => ({ client: {}, wsClient: {} })),
  disposeApolloClient: vi.fn(async () => undefined),
  controlPlaneQuery: vi.fn(),
}));
vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({ envVars: state.envVars }),
}));
vi.mock('@hasura/console-legacy-ce', () => ({
  globals: {},
  EndpointNamedExps: {},
  Endpoints: {},
  ControlPlane: { controlPlaneClient: { query: state.controlPlaneQuery } },
}));
vi.mock('../../../shared/auth/context', () => ({
  useAuthContext: () => state.auth,
}));
vi.mock('./LeftPanel', () => ({
  default: () => <div>Metrics panel</div>,
}));
vi.mock('./RightPanel', () => ({
  default: ({ children }: { children: React.ReactNode }) => (
    <div>{children}</div>
  ),
}));
vi.mock('../../Login/LoginWithHasura', () => ({
  default: () => <div>Hasura login</div>,
}));
vi.mock('../../../apollo.config', () => ({
  makeApolloClient: state.makeApolloClient,
  disposeApolloClient: state.disposeApolloClient,
}));
vi.mock('./utils', () => ({
  getMetricsUrl: (host: string) =>
    `${host.startsWith('http') ? host : `https://${host}`}/v1/graphql`,
}));
vi.mock('@apollo/client/react', () => ({
  ApolloProvider: ({ children }: { children: React.ReactNode }) => (
    <>{children}</>
  ),
}));
vi.mock('@hasura/shared/ui', () => ({
  Text: ({ children }: { children: React.ReactNode }) => (
    <span>{children}</span>
  ),
  Link: ({ children }: { children: React.ReactNode }) => <a>{children}</a>,
  Button: ({ children }: { children: React.ReactNode }) => (
    <button>{children}</button>
  ),
}));
vi.mock('@radix-ui/themes', () => ({
  Skeleton: () => <span>Loading metrics</span>,
  Code: ({ children }: { children: React.ReactNode }) => (
    <code>{children}</code>
  ),
}));
vi.mock('react-router', () => ({
  Outlet: () => <div>Metrics outlet</div>,
}));

let queryClient: QueryClient;
const wrapper = ({ children }: { children: React.ReactNode }) => (
  <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
);
const projectInfo = async () => {
  const hook = renderHook(() => useProjectInfo(), { wrapper });
  await waitFor(() => expect(hook.result.current.isSuccess).toBe(true));
  return hook;
};
// Unsigned token: the console only decodes the claims, it never verifies them.
const makeIdToken = (claims: Record<string, unknown>) =>
  ['{"alg":"none"}', JSON.stringify(claims)]
    .map((part) => btoa(part).replace(/=+$/, ''))
    .join('.') + '.';
const tokenClaims = (overrides: Record<string, unknown> = {}) => ({
  sub: 'user-1',
  aud: [`${PROJECT}_console`],
  collaborator_privileges: ['admin'],
  project: { id: PROJECT, name: 'prod-data' },
  ...overrides,
});
const deferred = () => {
  let resolve!: (headers: Record<string, string>) => void;
  const promise = new Promise<Record<string, string>>((done) => {
    resolve = done;
  });
  return { promise, resolve };
};
beforeEach(() => {
  vi.clearAllMocks();
  queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
  state.envVars = {
    consoleMode: 'server',
    consoleType: 'pro',
    hasuraOAuthUrl: 'https://oauth.example.com',
    hasuraMetricsUrl: 'https://metrics.example.com/',
  };
  window.__env = state.envVars as typeof window.__env;
  state.auth = {
    isAuthenticated: true,
    authType: 'hasura-sso',
    hasuraUserId: 'user-1',
    privileges: ['admin'],
    idToken: makeIdToken(tokenClaims()),
    getMetricsHeaders: vi.fn(async () => ({
      Authorization: 'Bearer test-only',
    })),
    logout: vi.fn(),
  };
});
afterEach(() => queryClient.clear());

describe('self-hosted Pro monitoring', () => {
  it('renders monitoring after Hasura SSO without a Cloud projectID', async () => {
    render(<Metrics />, { wrapper });
    await waitFor(() =>
      expect(screen.queryByText('Metrics panel')).not.toBeNull(),
    );
    expect(screen.queryByText(/HASURA_GRAPHQL_PRO_KEY/)).toBeNull();
    expect(state.controlPlaneQuery).not.toHaveBeenCalled();
    expect(state.makeApolloClient).toHaveBeenCalledWith(
      'https://metrics.example.com/v1/graphql',
      { Authorization: 'Bearer test-only' },
    );
  });

  it('decodes the project and privileges from the Hasura id token', async () => {
    const { result } = await projectInfo();
    expect(result.current.data).toMatchObject({
      id: PROJECT,
      metricsFQDN: 'https://metrics.example.com',
      privileges: ['admin'],
    });
  });

  it('prefers the token project to a configured projectID', async () => {
    state.envVars.projectID = OTHER;
    const { result } = await projectInfo();
    expect(result.current.data?.id).toBe(PROJECT);
  });

  it('falls back to the token metrics host', async () => {
    delete state.envVars.hasuraMetricsUrl;
    delete state.envVars.hasuraOAuthUrl;
    state.auth.idToken = makeIdToken(
      tokenClaims({ metrics_fqdn: 'token-metrics.example.com' }),
    );
    const { result } = await projectInfo();
    expect(result.current.data?.metricsFQDN).toBe('token-metrics.example.com');
  });

  it.each([
    [
      'without a project claim',
      makeIdToken(tokenClaims({ project: undefined })),
    ],
    ['that cannot be decoded', 'not-a-jwt'],
  ])('returns no project for a token %s', async (_, idToken) => {
    state.auth.idToken = idToken;
    const { result } = await projectInfo();
    expect(result.current.data).toBeNull();
  });

  it.each(['admin-secret', 'sso'])(
    'asks %s users to sign in with Hasura, without creating an anonymous metrics client',
    async (authType) => {
      state.auth.authType = authType;
      render(<Metrics />, { wrapper });
      await waitFor(() =>
        expect(screen.queryByText('Hasura login')).not.toBeNull(),
      );
      expect(state.auth.getMetricsHeaders).not.toHaveBeenCalled();
      expect(state.makeApolloClient).not.toHaveBeenCalled();
      expect(screen.queryByText(/HASURA_GRAPHQL_PRO_KEY/)).toBeNull();
    },
  );

  it('ignores the token for other auth types', async () => {
    state.auth.authType = 'admin-secret';
    state.envVars.projectID = OTHER;
    const { result } = await projectInfo();
    expect(result.current.data).toMatchObject({ id: OTHER });
    expect(result.current.data?.privileges).toBeUndefined();
  });

  it('uses the token privileges without granting admin', async () => {
    state.auth.idToken = makeIdToken(
      tokenClaims({ collaborator_privileges: ['view_metrics'] }),
    );
    const { result } = await projectInfo();
    expect(result.current.data?.privileges).toEqual(['view_metrics']);
  });

  it('supports metrics PAT headers', async () => {
    state.auth.authType = 'pat';
    state.envVars.projectID = PROJECT;
    state.auth.getMetricsHeaders.mockResolvedValue({
      Authorization: 'pat test-only',
    });
    render(<Metrics />, { wrapper });
    await waitFor(() =>
      expect(screen.queryByText('Metrics panel')).not.toBeNull(),
    );
    expect(state.makeApolloClient).toHaveBeenCalledWith(
      'https://metrics.example.com/v1/graphql',
      { Authorization: 'pat test-only' },
    );
  });

  it('shows the configuration hint if no project can be resolved', async () => {
    state.auth.idToken = makeIdToken(tokenClaims({ project: undefined }));
    render(<Metrics />, { wrapper });
    await waitFor(() =>
      expect(screen.queryByText(/HASURA_GRAPHQL_PRO_KEY/)).not.toBeNull(),
    );
    expect(state.makeApolloClient).not.toHaveBeenCalled();
  });

  it('handles rejected metrics headers without creating a client', async () => {
    state.auth.getMetricsHeaders.mockRejectedValue(
      new Error('session expired'),
    );
    render(<Metrics />, { wrapper });
    await waitFor(() =>
      expect(screen.queryByText(/Something went wrong/)).not.toBeNull(),
    );
    expect(state.makeApolloClient).not.toHaveBeenCalled();
  });

  it('ignores an in-flight header result after unmount', async () => {
    const headers = deferred();
    state.auth.getMetricsHeaders.mockReturnValue(headers.promise);
    const { unmount } = render(<Metrics />, { wrapper });
    await waitFor(() =>
      expect(state.auth.getMetricsHeaders).toHaveBeenCalledOnce(),
    );
    unmount();
    await act(async () => headers.resolve({ Authorization: 'Bearer stale' }));
    expect(state.makeApolloClient).not.toHaveBeenCalled();
  });

  it('disposes a created client on unmount', async () => {
    const { unmount } = render(<Metrics />, { wrapper });
    await waitFor(() => expect(state.makeApolloClient).toHaveBeenCalledOnce());
    unmount();
    expect(state.disposeApolloClient).toHaveBeenCalledWith(
      state.makeApolloClient.mock.results[0].value,
    );
  });

  it('does not reuse a client or late headers after the session changes', async () => {
    const oldHeaders = deferred();
    state.auth.getMetricsHeaders.mockReturnValue(oldHeaders.promise);
    const { rerender } = render(<Metrics />, { wrapper });
    await waitFor(() =>
      expect(state.auth.getMetricsHeaders).toHaveBeenCalledOnce(),
    );
    state.auth = {
      ...state.auth,
      getMetricsHeaders: vi.fn(async () => ({ Authorization: 'Bearer new' })),
    };
    rerender(<Metrics />);
    await waitFor(() => expect(state.makeApolloClient).toHaveBeenCalledOnce());
    await act(async () =>
      oldHeaders.resolve({ Authorization: 'Bearer stale' }),
    );
    expect(state.makeApolloClient).toHaveBeenCalledOnce();
    expect(state.makeApolloClient).toHaveBeenCalledWith(
      'https://metrics.example.com/v1/graphql',
      { Authorization: 'Bearer new' },
    );
  });
});

describe('project cache and other console modes', () => {
  it('does not reuse project metadata after the token changes', async () => {
    const { result, rerender } = await projectInfo();
    expect(result.current.data?.id).toBe(PROJECT);
    state.auth = {
      ...state.auth,
      idToken: makeIdToken(
        tokenClaims({
          sub: 'user-2',
          project: { id: OTHER },
          collaborator_privileges: ['view_metrics'],
        }),
      ),
    };
    rerender();
    await waitFor(() => expect(result.current.data?.id).toBe(OTHER));
    expect(result.current.data?.privileges).toEqual(['view_metrics']);
  });

  it('retains the Cloud control-plane project lookup', async () => {
    state.envVars.consoleType = 'cloud';
    state.envVars.projectID = PROJECT;
    state.controlPlaneQuery.mockResolvedValue({
      data: {
        projects_by_pk: {
          id: PROJECT,
          owner: { id: 'user-1' },
          tenant: {
            region_info: { metrics_fqdn: 'cloud-metrics.example.com' },
          },
          plan_name: 'cloud',
          entitlements: [],
        },
        users: [{ id: 'user-1' }],
      },
    });
    const { result } = await projectInfo();
    expect(result.current.data).toMatchObject({
      id: PROJECT,
      metricsFQDN: 'cloud-metrics.example.com',
      privileges: ['admin', 'graphql_admin', 'view_metrics'],
    });
    expect(state.controlPlaneQuery).toHaveBeenCalledWith(undefined, {
      id: PROJECT,
    });
  });

  it('does not infer a Cloud project from a Pro session', async () => {
    state.envVars.consoleType = 'cloud';
    const { result } = await projectInfo();
    expect(result.current.data).toBeNull();
    expect(state.controlPlaneQuery).not.toHaveBeenCalled();
  });

  it.each(['oss', 'cli'])(
    'retains the %s project-info exclusion',
    async (mode) => {
      if (mode === 'cli') state.envVars.consoleMode = 'cli';
      else state.envVars.consoleType = 'oss';
      const { result } = await projectInfo();
      expect(result.current.data).toBeNull();
      expect(state.controlPlaneQuery).not.toHaveBeenCalled();
    },
  );

  it('retains the CLI non-PAT login prompt', async () => {
    state.envVars.consoleMode = 'cli';
    render(<Metrics />, { wrapper });
    await waitFor(() =>
      expect(screen.queryByText('Login with PAT')).not.toBeNull(),
    );
    expect(state.makeApolloClient).not.toHaveBeenCalled();
  });
});

describe('self-hosted metrics URL configuration', () => {
  it('uses a configured metrics URL without projectID', () => {
    expect(getHasuraMetricsUrl(state.envVars as any)).toBe(
      'https://metrics.example.com',
    );
  });
  it('uses the existing OAuth-derived URL fallback', () => {
    delete state.envVars.hasuraMetricsUrl;
    expect(getHasuraMetricsUrl(state.envVars as any)).toBe(
      'https://metrics.example.com',
    );
  });
  it.each(['cloud', 'oss', 'cli'])(
    'does not apply the Pro URL fallback to %s',
    (mode) => {
      if (mode === 'cli') state.envVars.consoleMode = 'cli';
      else state.envVars.consoleType = mode;
      expect(getHasuraMetricsUrl(state.envVars as any)).toBe('');
    },
  );
});
