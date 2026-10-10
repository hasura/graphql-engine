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
  getHasuraSsoIdentityProvider,
  getOAuthRedirectUrl,
  makeHasuraSsoAuthState,
  makeSsoAuthState,
  retrieveByRefreshToken,
  shouldRefreshAuthState,
} from './utils';
import useNavigateAuth from './useNavigateAuth';
import { useNavigate } from 'react-router';
import { useQueryClient } from '@tanstack/react-query';
import {
  CLI_CONSOLE_MODE,
  LOGIN_PATH,
  ProServerEnv,
} from '@hasura/shared/types';
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

  // header getters may be called from stale closures and concurrently,
  // so track the latest auth state and the in-flight refresh request in refs.
  const authStateRef = useRef<EnterpriseAuthState | null>(null);
  const refreshPromiseRef = useRef<Promise<EnterpriseAuthState> | null>(null);
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

  // exchange the refresh token of an SSO session for a new auth state.
  // Throws if the session can't be refreshed.
  const refreshSsoAuthState = async (
    state: EnterpriseAuthState,
  ): Promise<EnterpriseAuthState> => {
    if (state.type !== 'sso' && state.type !== 'hasura-sso') {
      return state;
    }

    if (!state.refreshToken) {
      throw new Error(
        'the session has expired and no refresh token is present',
      );
    }

    const provider =
      state.type === 'hasura-sso'
        ? getHasuraSsoIdentityProvider(envVars)
        : (envVars as ProServerEnv).ssoIdentityProviders?.find(
            (idp) => idp.client_id === state.clientId,
          );

    if (!provider) {
      throw new Error('unable to find the identity provider of the session');
    }

    const redirectUri = getOAuthRedirectUrl(envVars.urlPrefix);
    const data = await retrieveByRefreshToken(
      provider,
      state.refreshToken,
      redirectUri,
    );
    const newState =
      state.type === 'hasura-sso'
        ? makeHasuraSsoAuthState(data, envVars)
        : makeSsoAuthState(provider, data);

    if (!newState || newState.type !== state.type) {
      throw new Error('invalid token response from the identity provider');
    }

    // identity providers may not rotate the refresh token
    return {
      ...newState,
      refreshToken: newState.refreshToken ?? state.refreshToken,
    };
  };

  const initialize = async () => {
    // Admin secret is set globally by the CLI.
    let authState = loadConsoleAuthState<EnterpriseAuthState>();
    if (!authState) {
      if (globals.isAdminSecretSet || !globals.adminSecret) {
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
        authState = await refreshSsoAuthState(authState);
      } catch (err) {
        console.error('failed to refresh the access token', err);
        clearConsoleAuthState();
        navigateAuth(null);
        return;
      }
    }

    await authenticate(authState).then((isAuthenticated) =>
      navigateAuth(isAuthenticated ? authState : null),
    );
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

  const refreshAuthState = (state: EnterpriseAuthState) => {
    if (!refreshPromiseRef.current) {
      refreshPromiseRef.current = refreshSsoAuthState(state)
        .then((newState) => {
          setAuthState(newState);
          if (envVars.consoleMode !== CLI_CONSOLE_MODE) {
            saveConsoleAuthState(newState);
          }

          return newState;
        })
        .finally(() => {
          refreshPromiseRef.current = null;
        });
    }

    return refreshPromiseRef.current;
  };

  // return the current auth state, refreshing the SSO token if it's about to expire.
  // Clears the session and redirects to the login page if the refresh fails.
  const getFreshAuthState = async () => {
    const state = authStateRef.current;
    if (!state || !shouldRefreshAuthState(state)) {
      return state;
    }

    try {
      return await refreshAuthState(state);
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
