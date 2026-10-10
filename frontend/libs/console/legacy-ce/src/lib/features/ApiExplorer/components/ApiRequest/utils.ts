import { ADMIN_SECRET_HEADER_KEY } from '@hasura/shared/types';
import { setLSItem, getLSItem, removeLSItem } from '@hasura/shared/utils';
import type { DataHeader } from '@hasura/shared/types';
import { JwtHeader, JwtPayload } from 'jwt-decode';
import { LS_KEYS } from '@hasura/shared/types';

export const setHeadersSectionIsOpen = (isOpen) => {
  setLSItem(LS_KEYS.apiExplorerHeaderSectionIsOpen, isOpen);
};

export const getHeadersSectionIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = getLSItem(LS_KEYS.apiExplorerHeaderSectionIsOpen);

  return isOpen ? isOpen === 'true' : defaultIsOpen;
};

export const persistAdminSecretHeaderWasAdded = () => {
  setLSItem(LS_KEYS.apiExplorerAdminSecretWasAdded, 'true');
};

export const removePersistedAdminSecretHeaderWasAdded = () => {
  removeLSItem(LS_KEYS.apiExplorerAdminSecretWasAdded);
};

export const getPersistedAdminSecretHeaderWasAdded = () => {
  const lsValue = getLSItem(LS_KEYS.apiExplorerAdminSecretWasAdded);

  return lsValue ? lsValue === 'true' : false;
};

export const persistGraphiQLHeaders = (headers: DataHeader[]) => {
  // filter empty headers
  const validHeaders = headers.filter((h) => h.key);

  // remove admin-secret value
  const maskedHeaders = validHeaders.map((h) => {
    const maskedHeader = { ...h };

    if (h.key.toLowerCase() === ADMIN_SECRET_HEADER_KEY) {
      maskedHeader.value = 'xxx';
    }

    return maskedHeader;
  });

  setLSItem(
    LS_KEYS.apiExplorerConsoleGraphQLHeaders,
    JSON.stringify(maskedHeaders),
  );
};

export const getPersistedGraphiQLHeaders = (
  authHeaders: Record<string, string>,
) => {
  const headersString = getLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders);

  let headers: DataHeader[] = [];
  const usedAuthHeaderKeys: string[] = [];

  if (headersString) {
    try {
      const persistedHeaders = JSON.parse(headersString);
      // add admin-secret value
      headers = persistedHeaders
        .filter((h) => h.key)
        .map((h) => {
          if (authHeaders[h.key]) {
            h.value = authHeaders[h.key];
            usedAuthHeaderKeys.push(h.key);
          }

          return h;
        });
    } catch (_) {
      console.error('Failed parsing headers from local storage');
    }
  }

  Object.entries(authHeaders).forEach(([key, value]) => {
    if (usedAuthHeaderKeys.includes(key)) {
      return;
    }

    headers.push({
      key,
      value,
      selected: true,
      isDisabled: true,
    });
  });

  return headers;
};

export const getDefaultGraphiqlHeaders = (): DataHeader[] => {
  return [
    {
      key: 'content-type',
      value: 'application/json',
      selected: true,
      isDisabled: false,
    },
  ];
};

export const parseAuthHeader = (header) => {
  let isAuthHeader = false;
  let token: string | null = null;

  if (header.key.toLowerCase() === 'authorization') {
    const parseBearer = /^(Bearer) (.*)/gm;
    const matches = parseBearer.exec(header.value);
    if (matches) {
      isAuthHeader = true;
      token = matches[2];
    }
  }

  return { isAuthHeader, token };
};

export const persistGraphiQLMode = (mode) => {
  setLSItem(LS_KEYS.apiExplorerGraphiqlMode, mode);
};

export const getPersistedGraphiQLMode = () => {
  return getLSItem(LS_KEYS.apiExplorerGraphiqlMode) || 'graphql';
};

export const getGraphiQLQueryFromLocalStorage = () => {
  return getLSItem(LS_KEYS.graphiqlQuery);
};

export const setGraphiQLQueryInLocalStorage = (query) => {
  return setLSItem(LS_KEYS.graphiqlQuery, query);
};

export type TokenInfo = {
  header?: JwtHeader | null;
  payload?: JwtPayload | null;
  error?: string;
};

export const createTokenInfo = (): TokenInfo => ({
  header: null,
  payload: null,
});
