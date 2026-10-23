import { useState } from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { MemoryRouter, Routes, Route, useNavigate } from 'react-router';
import { AuthContext } from '@hasura/shared/context';
import { AuthenticatedRoute } from './AuthenticatedRoute';

function Harness({ initialAuth = false }: { initialAuth?: boolean }) {
  const [isAuthenticated, setAuthenticated] = useState(initialAuth);
  const navigate = useNavigate();
  return (
    <AuthContext.Provider
      value={{
        isAuthenticated,
        authType: 'none',
        authenticate: vi.fn(),
        initialize: vi.fn(),
        getHeaders: vi.fn(),
        logout: () => setAuthenticated(false),
      }}
    >
      <button onClick={() => setAuthenticated(false)}>expire session</button>
      <button onClick={() => navigate('/data')}>open data</button>
      <Routes>
        <Route
          path="/data"
          element={
            <AuthenticatedRoute>
              <div>protected console</div>
            </AuthenticatedRoute>
          }
        />
        <Route path="/login" element={<div>login form</div>} />
        <Route path="/oauth/callback" element={<div>OAuth callback</div>} />
      </Routes>
    </AuthContext.Provider>
  );
}

it('does not render protected content without an authenticated session', async () => {
  render(
    <MemoryRouter initialEntries={['/data']}>
      <Harness />
    </MemoryRouter>,
  );
  expect(screen.queryByText('protected console')).not.toBeInTheDocument();
  expect(await screen.findByText('login form')).toBeInTheDocument();
});

it('unmounts protected content on session loss and prevents re-entry', async () => {
  render(
    <MemoryRouter initialEntries={['/data']}>
      <Harness initialAuth />
    </MemoryRouter>,
  );
  expect(screen.getByText('protected console')).toBeInTheDocument();
  fireEvent.click(screen.getByText('expire session'));
  expect(await screen.findByText('login form')).toBeInTheDocument();
  expect(screen.queryByText('protected console')).not.toBeInTheDocument();
  fireEvent.click(screen.getByText('open data'));
  expect(await screen.findByText('login form')).toBeInTheDocument();
  expect(screen.queryByText('protected console')).not.toBeInTheDocument();
});

it('leaves OAuth callback routes accessible without a session', () => {
  render(
    <MemoryRouter initialEntries={['/oauth/callback']}>
      <Harness />
    </MemoryRouter>,
  );
  expect(screen.getByText('OAuth callback')).toBeInTheDocument();
  expect(screen.queryByText('login form')).not.toBeInTheDocument();
});
