import {
  ADMIN_SECRET_HEADER_KEY,
  HASURA_COLLABORATOR_TOKEN,
  HASURA_SSO_TOKEN,
  SERVER_CONSOLE_MODE,
  DataHeader,
  LS_KEYS,
  EnvVars,
} from '@hasura/shared/types';
import { getLSItem, removeLSItem, setLSItem } from '@hasura/shared/utils';

export const getGlobalAdminSecret = (envVars: EnvVars) => {
  if (envVars.consoleMode === SERVER_CONSOLE_MODE && envVars.isAdminSecretSet) {
    const adminSecretFromLS = loadAdminSecretState();
    const adminSecretInGlobals = envVars.adminSecret;

    return adminSecretFromLS || adminSecretInGlobals;
  }

  return envVars.adminSecret;
};

export const getAdminSecretHeaders = (
  adminSecret?: string,
): Record<string, string> => {
  return adminSecret
    ? {
        [ADMIN_SECRET_HEADER_KEY]: adminSecret,
      }
    : {};
};

export const loadConsoleAuthState = <T>(): T | null => {
  try {
    const rawState = getLSItem(LS_KEYS.consoleAuthState);
    if (!rawState) {
      return null;
    }

    return JSON.parse(rawState);
  } catch {
    return null;
  }
};

export function saveConsoleAuthState<T extends Record<string, any>>(value: T) {
  setLSItem(LS_KEYS.consoleAuthState, JSON.stringify(value));
}

export const loadAdminSecretState = () => {
  const authState = loadConsoleAuthState<{ adminSecret?: string | null }>();
  if (
    authState &&
    typeof authState === 'object' &&
    'adminSecret' in authState
  ) {
    return authState.adminSecret;
  }

  return null;
};

export const saveAdminSecretState = (state: string) => {
  if (!state) {
    clearConsoleAuthState();
    return;
  }

  saveConsoleAuthState({
    type: 'admin-secret',
    adminSecret: state,
  });
};

export const clearConsoleAuthState = () => {
  removeLSItem(LS_KEYS.consoleAuthState);
  clearPersistedDataHeaders();
};

const clearPersistedDataHeaders = () => {
  const headersString = getLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders);
  if (!headersString) {
    return;
  }

  try {
    const persistedHeaders = JSON.parse(headersString) as DataHeader[];
    if (!persistedHeaders || !Array.isArray(persistedHeaders)) {
      removeLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders);
      return;
    }

    // add admin-secret value
    const headers = persistedHeaders.filter(
      (h) =>
        ![
          HASURA_COLLABORATOR_TOKEN,
          HASURA_SSO_TOKEN,
          ADMIN_SECRET_HEADER_KEY,
        ].includes(h.key.toLowerCase()),
    );

    setLSItem(
      LS_KEYS.apiExplorerConsoleGraphQLHeaders,
      JSON.stringify(headers),
    );
  } catch (_) {
    removeLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders);
  }
};
