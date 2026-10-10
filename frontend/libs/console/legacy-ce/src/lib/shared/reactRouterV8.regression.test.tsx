/**
 * Primitive-level regression coverage for the React Router v7 -> v8 upgrade
 * (and React 19).
 *
 * v8 removed the `react-router-dom` package: every DOM binding this app uses
 * now lives in `react-router` (see the upgrade in frontend/package.json). These
 * tests are intentionally **self-contained** (a synthetic route tree, no app
 * components) so a behavioural break in react-router itself is caught here as a
 * primitive — they are NOT a claim of CE/EE route parity. Real production route
 * declarations (get-route-builders, the real 404 page and the real auth guard)
 * are exercised separately in `reactRouterV8.realRoutes.regression.test.tsx`.
 *
 * Covered primitives under React 19 + react-router v8: deep-link param/search/
 * hash extraction, nested relative paths + `<Outlet>`, `<Navigate replace>`
 * history semantics (proven against a sentinel entry, with forward
 * restoration), programmatic back *and* forward via `useNavigate(delta)`, live
 * `useSearchParams` updates, and a React 19 `createRoot` mount/unmount.
 */
import { render, screen, act } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { createRoot } from 'react-dom/client';
import {
  MemoryRouter,
  Routes,
  Route,
  Outlet,
  Navigate,
  Link,
  useParams,
  useLocation,
  useNavigate,
  useSearchParams,
} from 'react-router';

function TableView() {
  const { source, table } = useParams();
  const { hash } = useLocation();
  const [searchParams] = useSearchParams();
  return (
    <div>
      <span data-testid="source">{source}</span>
      <span data-testid="table">{table}</span>
      <span data-testid="tab">{searchParams.get('tab') ?? 'none'}</span>
      <span data-testid="hash">{hash}</span>
    </div>
  );
}

function DataLayout() {
  // Relative links exercise nested-route path resolution (no leading slash).
  return (
    <div>
      <h1>data</h1>
      <Link to="sources/pg/tables/users">go to table</Link>
      <Outlet />
    </div>
  );
}

function renderAppAt(entries: string[], initialIndex?: number) {
  return render(
    <MemoryRouter initialEntries={entries} initialIndex={initialIndex}>
      <Routes>
        <Route path="/" element={<Navigate to="data" replace />} />
        <Route path="data" element={<DataLayout />}>
          <Route path="sources/:source/tables/:table" element={<TableView />} />
        </Route>
        <Route path="*" element={<div>not-found</div>} />
      </Routes>
    </MemoryRouter>,
  );
}

describe('react-router v8 declarative routing contract (React 19)', () => {
  it('resolves a direct deep link with path params, search and hash', () => {
    renderAppAt(['/data/sources/pg/tables/users?tab=permissions#fragment']);
    expect(screen.getByTestId('source')).toHaveTextContent('pg');
    expect(screen.getByTestId('table')).toHaveTextContent('users');
    expect(screen.getByTestId('tab')).toHaveTextContent('permissions');
    expect(screen.getByTestId('hash')).toHaveTextContent('#fragment');
  });

  it('applies <Navigate replace> so the redirected entry is not pushed onto history', async () => {
    const user = userEvent.setup();

    // A "nav" probe that shows the current path and can step through history.
    function NavProbe() {
      const navigate = useNavigate();
      const { pathname } = useLocation();
      return (
        <div>
          <span data-testid="path">{pathname}</span>
          <button onClick={() => navigate(-1)}>back</button>
          <button onClick={() => navigate(1)}>forward</button>
        </div>
      );
    }

    // History: ['/sentinel', '/'] starting at index 1 ('/'). The index route
    // redirects to '/home' with `replace`, so '/' is *replaced* (not pushed):
    // the stack becomes ['/sentinel', '/home'] at index 1.
    render(
      <MemoryRouter initialEntries={['/sentinel', '/']} initialIndex={1}>
        <Routes>
          <Route path="/" element={<Navigate to="/home" replace />} />
          <Route path="sentinel" element={<NavProbe />} />
          <Route path="home" element={<NavProbe />} />
        </Routes>
      </MemoryRouter>,
    );

    expect(screen.getByTestId('path')).toHaveTextContent('/home');

    // One step back must land on the sentinel — proving '/' was replaced, not
    // pushed (a push would require two back steps to reach the sentinel).
    await user.click(screen.getByRole('button', { name: 'back' }));
    expect(screen.getByTestId('path')).toHaveTextContent('/sentinel');

    // Forward restores the replaced destination ('/home'), confirming the
    // forward entry is '/home' and not the original '/'.
    await user.click(screen.getByRole('button', { name: 'forward' }));
    expect(screen.getByTestId('path')).toHaveTextContent('/home');
  });

  it('follows a nested relative <Link> to resolve child route params', async () => {
    const user = userEvent.setup();
    renderAppAt(['/data']);
    await user.click(screen.getByRole('link', { name: 'go to table' }));
    expect(screen.getByTestId('source')).toHaveTextContent('pg');
    expect(screen.getByTestId('table')).toHaveTextContent('users');
  });

  it('supports programmatic back and forward navigation via useNavigate(delta)', async () => {
    const user = userEvent.setup();
    function Probe() {
      const navigate = useNavigate();
      const { pathname } = useLocation();
      return (
        <div>
          <span data-testid="path">{pathname}</span>
          <button onClick={() => navigate('/data/sources/pg/tables/orders')}>
            push
          </button>
          <button onClick={() => navigate(-1)}>back</button>
          <button onClick={() => navigate(1)}>forward</button>
        </div>
      );
    }
    render(
      <MemoryRouter initialEntries={['/data']}>
        <Routes>
          <Route path="data" element={<Probe />} />
          <Route
            path="data/sources/:source/tables/:table"
            element={<Probe />}
          />
        </Routes>
      </MemoryRouter>,
    );
    expect(screen.getByTestId('path')).toHaveTextContent('/data');

    // push -> /orders, back (delta -1) -> /data, forward (delta +1) -> /orders.
    await user.click(screen.getByRole('button', { name: 'push' }));
    expect(screen.getByTestId('path')).toHaveTextContent(
      '/data/sources/pg/tables/orders',
    );
    await user.click(screen.getByRole('button', { name: 'back' }));
    expect(screen.getByTestId('path')).toHaveTextContent('/data');
    await user.click(screen.getByRole('button', { name: 'forward' }));
    expect(screen.getByTestId('path')).toHaveTextContent(
      '/data/sources/pg/tables/orders',
    );
  });

  it('reflects live search-param updates through useSearchParams', async () => {
    const user = userEvent.setup();
    function SearchProbe() {
      const [searchParams, setSearchParams] = useSearchParams();
      return (
        <div>
          <span data-testid="q">{searchParams.get('q') ?? 'empty'}</span>
          <button onClick={() => setSearchParams({ q: 'metadata' })}>
            search
          </button>
        </div>
      );
    }
    render(
      <MemoryRouter initialEntries={['/data']}>
        <Routes>
          <Route path="data" element={<SearchProbe />} />
        </Routes>
      </MemoryRouter>,
    );
    expect(screen.getByTestId('q')).toHaveTextContent('empty');
    await user.click(screen.getByRole('button', { name: 'search' }));
    expect(screen.getByTestId('q')).toHaveTextContent('metadata');
  });

  it('mounts and unmounts a routed tree through a React 19 createRoot (single-root lifecycle)', () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    act(() => {
      root.render(
        <MemoryRouter initialEntries={['/data/sources/pg/tables/users']}>
          <Routes>
            <Route
              path="data/sources/:source/tables/:table"
              element={<TableView />}
            />
          </Routes>
        </MemoryRouter>,
      );
    });
    expect(container.querySelector('[data-testid="table"]')?.textContent).toBe(
      'users',
    );
    act(() => {
      root.unmount();
    });
    expect(container).toBeEmptyDOMElement();
    container.remove();
  });
});
