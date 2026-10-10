/**
 * CE route-parity regression coverage for the React Router v7 -> v8 upgrade.
 *
 * Unlike the self-contained primitive tests in
 * `reactRouterV8.regression.test.tsx`, these execute **real production route
 * declarations and route helpers** so we have actual CE evidence (not a toy
 * route tree) that the v8 upgrade preserves the app's routing contracts:
 *
 *   - `getAllowListRoutes()` — a real get-route-builder from
 *     `features/AllowLists/routes`: its real index `<Navigate to="detail"
 *     replace>` default redirect and real `:name` / `:name/:section` param
 *     routes, including an encoded deep link.
 *   - `PageNotFound` — the real 404 page (and, through it, the real shared/ui
 *     `RelativeLink`) mounted at the catch-all `*`.
 *   - `useNavigateAuth` — the real auth-guard hook: the unauthenticated
 *     "denied" redirect to `LOGIN_PATH` and the authenticated-on-login redirect
 *     back to the app root (`globals.urlPrefix`).
 *
 * MOCKS (clearly labelled): only the *leaf* feature page `AllowListDetail` is
 * stubbed — it is a heavy page (EE-trial gating, data hooks, shared/ui) and is
 * not what we are validating. The stub echoes its route params so we can assert
 * the REAL route declarations matched and decoded params correctly. No route
 * tree, redirect, 404 page, or guard logic is mocked or duplicated here.
 */
import { render, screen, waitFor } from '@testing-library/react';
import { useEffect, useRef } from 'react';
import {
  MemoryRouter,
  Routes,
  Route,
  useLocation,
  useParams,
} from 'react-router';

// --- MOCK (leaf page only) -------------------------------------------------
vi.mock('../features/AllowLists/components/AllowListDetail', () => ({
  AllowListDetail: () => {
    const { name, section } = useParams();
    return (
      <div
        data-testid="allowlist-detail"
        data-name={name ?? ''}
        data-section={section ?? ''}
      />
    );
  },
}));

// --- REAL production code under test ---------------------------------------
import getAllowListRoutes from '../features/AllowLists/routes';
import PageNotFound from '../components/Error/PageNotFound';
import useNavigateAuth from './auth/useNavigateAuth';

(window as any).__env = (window as any).__env ?? {};

function LocationProbe() {
  const { pathname } = useLocation();
  return <span data-testid="path">{pathname}</span>;
}

function renderRealAllowListRoutes(entry: string) {
  // The REAL getAllowListRoutes() subtree is mounted verbatim alongside the
  // REAL PageNotFound catch-all, mirroring how Router.tsx composes them.
  return render(
    <MemoryRouter initialEntries={[entry]}>
      <LocationProbe />
      <Routes>
        {getAllowListRoutes()}
        <Route path="*" element={<PageNotFound />} />
      </Routes>
    </MemoryRouter>,
  );
}

describe('CE real route declarations under react-router v8', () => {
  it('applies the real index default-redirect (allow-list -> allow-list/detail, replace)', () => {
    renderRealAllowListRoutes('/allow-list');
    expect(screen.getByTestId('path')).toHaveTextContent('/allow-list/detail');
    expect(screen.getByTestId('allowlist-detail')).toBeInTheDocument();
  });

  it('matches the real encoded :name/:section deep link and decodes params', () => {
    renderRealAllowListRoutes('/allow-list/detail/my%20list/permissions');
    const el = screen.getByTestId('allowlist-detail');
    // react-router decodes path params: "my%20list" -> "my list".
    expect(el).toHaveAttribute('data-name', 'my list');
    expect(el).toHaveAttribute('data-section', 'permissions');
    expect(screen.getByTestId('path')).toHaveTextContent(
      '/allow-list/detail/my%20list/permissions',
    );
  });

  it('renders the real PageNotFound (404) for an unknown path', () => {
    renderRealAllowListRoutes('/allow-list/does-not-exist/deeper');
    expect(screen.getByRole('heading', { name: '404' })).toBeInTheDocument();
    expect(screen.getByText(/This page does not exist/i)).toBeInTheDocument();
    expect(screen.queryByTestId('allowlist-detail')).not.toBeInTheDocument();
  });
});

describe('CE real auth-guard contract (useNavigateAuth) under react-router v8', () => {
  function GuardHarness({ authed }: { authed: boolean }) {
    const runGuard = useNavigateAuth();
    const ran = useRef(false);
    useEffect(() => {
      if (!ran.current) {
        ran.current = true;
        runGuard(authed);
      }
    }, [runGuard, authed]);
    const { pathname } = useLocation();
    return <span data-testid="path">{pathname}</span>;
  }

  function renderGuard(entry: string, authed: boolean) {
    return render(
      <MemoryRouter initialEntries={[entry]}>
        <Routes>
          <Route path="*" element={<GuardHarness authed={authed} />} />
        </Routes>
      </MemoryRouter>,
    );
  }

  it('redirects an unauthenticated visit on a protected path to LOGIN_PATH (denied case)', async () => {
    renderGuard('/data/manage', false);
    await waitFor(() =>
      expect(screen.getByTestId('path')).toHaveTextContent('/login'),
    );
  });

  it('redirects an authenticated visit on the login path back to the app root', async () => {
    renderGuard('/login', true);
    await waitFor(() =>
      // globals.urlPrefix defaults to '' (no __env) -> guard navigates to '/'.
      // Exact match (/^\/$/) so this can't pass on the original '/login'.
      expect(screen.getByTestId('path')).toHaveTextContent(/^\/$/),
    );
  });
});
