import {
  clearConsoleAuthState,
  globals,
  loadConsoleAuthState,
  Privilege,
  saveAdminSecretState,
  saveConsoleAuthState,
} from '@hasura/console-legacy-ce';
import { useCallback, useRef, useState } from 'react';
import type { EnterpriseAuthService, EnterpriseAuthState } from './types';
import {
  buildEnterpriseAuthHeaders,
  buildMetricsAuthHeaders,
  shouldRefreshAuthState,
} from './utils';
import { refreshSsoSession } from './refreshSession';
import useNavigateAuth from './useNavigateAuth';
import { useNavigate } from 'react-router';
import { useQueryClient } from '@tanstack/react-query';
import { CLI_CONSOLE_MODE, LOGIN_PATH } from '@hasura/shared/types';
import { useFetchInconsistentMetadata } from '@hasura/metadata/api';
import { useAppContext } from '@hasura/shared/context';

const useEnterpriseAuth = (): EnterpriseAuthService => {
  const navigate = useNavigate();
  const navigateAuth = useNavigateAuth();
  const queryClient = useQueryClient();
  const fetchInconsistentMetadata = useFetchInconsistentMetadata();
  const [authState, setAuthStateValue] = useState<EnterpriseAuthState | null>(
    null,
  );
  const { envVars } = useAppContext();

  // header getters may be called from stale closures, so track the latest auth state in a ref.
  const authStateRef = useRef<EnterpriseAuthState | null>(null);
  const navigateAuthRef = useRef(navigateAuth);
  navigateAuthRef.current = navigateAuth;

  const setAuthState = useCallback((state: EnterpriseAuthState | null) => {
    authStateRef.current = state;
    setAuthStateValue(state);
  }, []);

  const authenticate = useCallback(
    async (input: EnterpriseAuthState) => {
      const headers = buildEnterpriseAuthHeaders(input);

      await fetchInconsistentMetadata(headers);
      setAuthState(input);

      if (envVars.consoleMode === CLI_CONSOLE_MODE) {
        return true;
      }

      // set the auth state to local storage
      switch (input.type) {
        case 'admin-secret':
          if (input.shouldPersist) {
            saveAdminSecretState(input.adminSecret);
          }
          break;
        case 'hasura-sso':
        case 'sso':
          // save access token anr optional refresh token.
          saveConsoleAuthState(input);
      }

      return true;
    },
    [setAuthState],
  );

  const initialize = async () => {
    // Cloud and CLI consoles can inject an admin secret through window.__env.
    // Prefer it over a credential saved in local storage (e.g. from an earlier
    // session on the same origin).
    const injectedAdminSecret =
      envVars.consoleMode === CLI_CONSOLE_MODE ||
      envVars.consoleType === 'cloud'
        ? envVars.adminSecret
        : '';
    let authState: EnterpriseAuthState | null = injectedAdminSecret
      ? {
          type: 'admin-secret',
          adminSecret: injectedAdminSecret,
          shouldPersist: false,
        }
      : loadConsoleAuthState<EnterpriseAuthState>();
    // Without an injected secret, a saved admin secret, PAT, or third-party
    // SSO session must not bypass Hasura OAuth in a cloud console.
    if (
      !injectedAdminSecret &&
      envVars.consoleType === 'cloud' &&
      authState?.type !== 'hasura-sso'
    ) {
      clearConsoleAuthState();
      navigateAuth(null);
      return;
    }

    if (!authState) {
      // An injected secret also sets isAdminSecretSet. Only prompt for
      // credentials when the engine requires a secret and none is available.
      if (globals.isAdminSecretSet && !globals.adminSecret) {
        navigateAuth(null);
        return;
      }

      if (globals.adminSecret) {
        authState = {
          type: 'admin-secret',
          adminSecret: globals.adminSecret,
          shouldPersist: false,
        };
      } else {
        authState = {
          type: 'none',
        };
      }
    }

    // the persisted SSO token may have expired while the console was closed
    if (shouldRefreshAuthState(authState)) {
      try {
        authState = await refreshSsoSession(authState, envVars);
      } catch (err) {
        console.error('failed to refresh the access token', err);
        clearConsoleAuthState();
        navigateAuth(null);
        return;
      }
    }

    await authenticate(authState)
      .then((isAuthenticated) =>
        navigateAuth(isAuthenticated ? authState : null),
      )
      .catch(() => {
        clearConsoleAuthState();
        setAuthState(null);
        navigateAuth(null);
      });
  };

  const logout = () => {
    clearConsoleAuthState();
    setAuthState(null);
    queryClient.clear();
    navigate(LOGIN_PATH);
  };

  // the refresh token is invalid or expired, the user needs to login again.
  const expireSession = () => {
    clearConsoleAuthState();
    setAuthState(null);
    queryClient.clear();
    navigateAuthRef.current(null);
  };

  // return the current auth state, refreshing the SSO token if it's about to expire.
  // Clears the session and redirects to the login page if the refresh fails.
  const getFreshAuthState = async () => {
    const state = authStateRef.current;
    if (!state || !shouldRefreshAuthState(state)) {
      return state;
    }

    try {
      // concurrent callers share a single refresh request
      const newState = await refreshSsoSession(state, envVars);
      setAuthState(newState);

      return newState;
    } catch (err) {
      console.error('failed to refresh the access token', err);
      expireSession();
      throw Object.assign(
        new Error('Your session has expired. Please log in again.'),
        { cause: err },
      );
    }
  };

  const privileges =
    authState?.type === 'hasura-sso'
      ? (authState.project?.privileges ?? [])
      : (['admin'] as Privilege[]);

  // the identity changes whenever the auth state changes (e.g. after a token refresh)
  // so consumers that cache headers (the metrics apollo client) can rebuild.
  const getHeaders = useCallback(
    async () => buildEnterpriseAuthHeaders(await getFreshAuthState()),
    [authState],
  );
  const getMetricsHeaders = useCallback(
    async () => buildMetricsAuthHeaders(await getFreshAuthState()),
    [authState],
  );

  return {
    authenticate,
    isAuthenticated: Boolean(authState),
    getHeaders,
    getMetricsHeaders,
    authType: authState?.type ?? 'none',
    hasuraUserId: getHasuraUserId(authState),
    privileges,
    idToken: authState?.type === 'hasura-sso' ? authState.idToken : undefined,
    initialize,
    logout,
  };
};

const getHasuraUserId = (authState: EnterpriseAuthState | null) => {
  if (authState?.type === 'hasura-sso') {
    return authState.userId;
  }

  return undefined;
};

export default useEnterpriseAuth;
