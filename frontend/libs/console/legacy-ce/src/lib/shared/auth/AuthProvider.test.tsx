import { render, screen } from '@testing-library/react';
import { AppTheme } from '@hasura/shared/ui';

// Regression for the rules-of-hooks fix: the provider must set up exactly ONE
// auth service (never both). We mock each auth hook with a distinct
// initialize() spy and assert only the selected one is ever initialized.
const h = vi.hoisted(() => ({
  initAdmin: vi.fn(() => Promise.resolve()),
  initNoAuth: vi.fn(() => Promise.resolve()),
  isAdminSecretSet: true,
}));

vi.mock('./useAdminSecretAuth', () => ({
  useAdminSecretAuth: () => ({ initialize: h.initAdmin }),
}));
vi.mock('./useNoAuth', () => ({
  useNoAuth: () => ({ initialize: h.initNoAuth }),
}));
vi.mock('@hasura/shared/context', async (importOriginal) => {
  const actual =
    await importOriginal<typeof import('@hasura/shared/context')>();
  return {
    ...actual,
    useAppContext: () =>
      ({ envVars: { isAdminSecretSet: h.isAdminSecretSet } }) as any,
  };
});

import AuthProvider from './AuthProvider';

describe('AuthProvider', () => {
  beforeEach(() => {
    h.initAdmin.mockClear();
    h.initNoAuth.mockClear();
  });

  it('shows the validating screen, then initializes ONLY admin-secret auth when the admin secret is set', async () => {
    h.isAdminSecretSet = true;
    // LoadingScreen reads the appearance, so render under AppTheme like the app does.
    render(
      <AppTheme>
        <AuthProvider>
          <div>protected app</div>
        </AuthProvider>
      </AppTheme>,
    );

    expect(screen.getByText('Validating...')).toBeInTheDocument();
    expect(await screen.findByText('protected app')).toBeInTheDocument();

    expect(h.initAdmin).toHaveBeenCalledTimes(1);
    expect(h.initNoAuth).not.toHaveBeenCalled();
  });

  it('initializes ONLY no-auth when the admin secret is not set', async () => {
    h.isAdminSecretSet = false;
    // LoadingScreen reads the appearance, so render under AppTheme like the app does.
    render(
      <AppTheme>
        <AuthProvider>
          <div>protected app</div>
        </AuthProvider>
      </AppTheme>,
    );

    expect(await screen.findByText('protected app')).toBeInTheDocument();

    expect(h.initNoAuth).toHaveBeenCalledTimes(1);
    expect(h.initAdmin).not.toHaveBeenCalled();
  });
});
