import {
  loadConsoleAuthState,
  saveConsoleAuthState,
} from '@hasura/console-legacy-ce';
import { CLI_CONSOLE_MODE, EnvVars, ProServerEnv } from '@hasura/shared/types';
import type { EnterpriseAuthState } from './types';
import {
  getHasuraSsoIdentityProvider,
  getOAuthRedirectUrl,
  makeHasuraSsoAuthState,
  makeSsoAuthState,
  retrieveByRefreshToken,
  shouldRefreshAuthState,
} from './utils';

type SsoSessionState = Extract<
  EnterpriseAuthState,
  { type: 'sso' | 'hasura-sso' }
>;

// serializes refreshes across browser tabs, which share the session in local storage
const REFRESH_LOCK_NAME = 'hasura-console:sso-token-refresh';

// The identity provider rejects concurrent requests that use the same refresh token,
// and a rotated refresh token can't be used again. Every caller in this tab
// (initialize, header getters, other hook instances) shares the latest refresh,
// so a caller still holding the old token gets the result instead of a second request.
let lastRefresh: {
  refreshToken: string;
  promise: Promise<EnterpriseAuthState>;
} | null = null;

const withRefreshLock = <T>(fn: () => Promise<T>): Promise<T> =>
  typeof navigator !== 'undefined' && navigator.locks
    ? navigator.locks.request(REFRESH_LOCK_NAME, fn)
    : fn();

const isSameSession = (
  saved: EnterpriseAuthState | null,
  state: SsoSessionState,
): saved is SsoSessionState => {
  if (saved?.type !== state.type) {
    return false;
  }

  return (
    saved.type !== 'sso' ||
    state.type !== 'sso' ||
    saved.clientId === state.clientId
  );
};

const requestRefresh = async (
  state: SsoSessionState,
  refreshToken: string,
  envVars: EnvVars,
): Promise<EnterpriseAuthState> => {
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
    refreshToken,
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
    refreshToken: newState.refreshToken ?? refreshToken,
  };
};

const refreshOrAdoptSession = async (
  state: SsoSessionState,
  refreshToken: string,
  envVars: EnvVars,
): Promise<EnterpriseAuthState> => {
  // CLI consoles don't persist the session
  if (envVars.consoleMode === CLI_CONSOLE_MODE) {
    return requestRefresh(state, refreshToken, envVars);
  }

  // another tab may have refreshed the session while this one waited for the lock
  const saved = loadConsoleAuthState<EnterpriseAuthState>();
  if (
    isSameSession(saved, state) &&
    saved.refreshToken &&
    saved.refreshToken !== refreshToken &&
    !shouldRefreshAuthState(saved)
  ) {
    return saved;
  }

  const newState = await requestRefresh(state, refreshToken, envVars);
  // save before releasing the lock so other tabs pick up the rotated refresh token
  saveConsoleAuthState(newState);

  return newState;
};

// exchange the refresh token of an SSO session for a new auth state.
// Throws if the session can't be refreshed.
export const refreshSsoSession = (
  state: EnterpriseAuthState,
  envVars: EnvVars,
): Promise<EnterpriseAuthState> => {
  if (state.type !== 'sso' && state.type !== 'hasura-sso') {
    return Promise.resolve(state);
  }

  const { refreshToken } = state;
  if (!refreshToken) {
    return Promise.reject(
      new Error('the session has expired and no refresh token is present'),
    );
  }

  if (lastRefresh?.refreshToken === refreshToken) {
    return lastRefresh.promise;
  }

  const entry = {
    refreshToken,
    promise: withRefreshLock(() =>
      refreshOrAdoptSession(state, refreshToken, envVars),
    ),
  };
  lastRefresh = entry;

  const forget = () => {
    if (lastRefresh === entry) {
      lastRefresh = null;
    }
  };
  entry.promise.then((newState) => {
    // without rotation, the same refresh token is valid for the next refresh
    if (
      newState.type === state.type &&
      'refreshToken' in newState &&
      newState.refreshToken === refreshToken
    ) {
      forget();
    }
  }, forget);

  return entry.promise;
};

// only for tests
export const resetRefreshSession = () => {
  lastRefresh = null;
};
