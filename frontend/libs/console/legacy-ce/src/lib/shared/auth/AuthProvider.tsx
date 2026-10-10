import {
  AuthContext,
  AuthService,
  useAppContext,
} from '@hasura/shared/context';
import { useAdminSecretAuth } from './useAdminSecretAuth';
import { useNoAuth } from './useNoAuth';
import { useEffect, useState } from 'react';
import { LoadingScreen, LoadingScreenTitle } from '@hasura/shared/ui';

/**
 * Shared gate: initializes the provided auth service exactly once and renders a
 * loading screen until it resolves, then exposes it via AuthContext. Kept
 * separate from the hook selection below so the auth hooks are always called
 * unconditionally (React rules of hooks) — rendering only one of the provider
 * components means only a single auth service is ever set up.
 */
const AuthGate = ({
  authContext,
  children,
}: {
  authContext: AuthService<any>;
  children: React.ReactNode;
}) => {
  const [initializing, setInitializing] = useState(true);

  useEffect(() => {
    if (!initializing) {
      return;
    }

    authContext.initialize().finally(() => {
      setInitializing(false);
    });
  }, [authContext, initializing]);

  return initializing ? (
    <LoadingScreen>
      <LoadingScreenTitle title="Validating..." />
    </LoadingScreen>
  ) : (
    <AuthContext.Provider value={authContext}>{children}</AuthContext.Provider>
  );
};

const AdminSecretAuthProvider = ({
  children,
}: {
  children: React.ReactNode;
}) => <AuthGate authContext={useAdminSecretAuth()}>{children}</AuthGate>;

const NoAuthProvider = ({ children }: { children: React.ReactNode }) => (
  <AuthGate authContext={useNoAuth()}>{children}</AuthGate>
);

const AuthProvider = ({ children }: { children: React.ReactNode }) => {
  const { envVars } = useAppContext();

  // `isAdminSecretSet` is fixed for the lifetime of the app, so the selected
  // provider never swaps and the chosen auth hook mounts once. Each provider
  // calls exactly one auth hook unconditionally.
  return envVars.isAdminSecretSet ? (
    <AdminSecretAuthProvider>{children}</AdminSecretAuthProvider>
  ) : (
    <NoAuthProvider>{children}</NoAuthProvider>
  );
};

export default AuthProvider;
