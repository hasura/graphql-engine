import { useCallback, useState } from 'react';
import { CLI_CONSOLE_MODE, UnauthorizedError } from '@hasura/shared/types';
import {
  clearConsoleAuthState,
  getAdminSecretHeaders,
  getGlobalAdminSecret,
  saveAdminSecretState,
} from './utils';
import {
  AdminSecretState,
  AuthService,
  useAppContext,
} from '@hasura/shared/context';
import useNavigateAuth from './useNavigateAuth';
import { useFetchInconsistentMetadata } from '@hasura/metadata/api';

export const useAdminSecretAuth = (): AuthService<AdminSecretState> => {
  const navigateAuth = useNavigateAuth();
  const fetchInconsistentMetadata = useFetchInconsistentMetadata();
  const [authState, setAuthState] = useState<AdminSecretState | null>(null);
  const { envVars } = useAppContext();

  const authenticate = useCallback(
    async (input: AdminSecretState) => {
      if (!input.adminSecret) {
        throw new UnauthorizedError('Admin secret is required');
      }

      const headers = getAdminSecretHeaders(input.adminSecret);
      await fetchInconsistentMetadata(headers);
      setAuthState(input);

      if (envVars.consoleMode === CLI_CONSOLE_MODE) {
        return true;
      }

      // set admin secret to local storage
      if (input.shouldPersist) {
        saveAdminSecretState(input.adminSecret);
      }

      return true;
    },
    [setAuthState],
  );

  const initialize = async () => {
    // Admin secret is set globally by the CLI.
    const adminSecret = getGlobalAdminSecret(envVars);
    if (!adminSecret) {
      navigateAuth(false);
      return;
    }

    await authenticate({
      type: 'admin-secret',
      adminSecret,
      shouldPersist: false,
    })
      .then(navigateAuth)
      .catch(() => navigateAuth(false));
  };

  const logout = () => {
    clearConsoleAuthState();
  };

  return {
    authenticate,
    isAuthenticated: Boolean(authState?.adminSecret),
    getHeaders: async () => getAdminSecretHeaders(authState?.adminSecret),
    authType: 'admin-secret',
    initialize,
    logout,
  };
};
