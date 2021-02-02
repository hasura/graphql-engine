import globals from '../../../../Globals';
import { loadAdminSecretState } from '../../../AppState';
import {
  ADMIN_SECRET_HEADER_KEY,
  SERVER_CONSOLE_MODE,
} from '../../../../constants';
import {
  setLSItem,
  getLSItem,
  removeLSItem,
  LS_KEYS,
} from '../../../../utils/localStorage';

export const setEndPointSectionIsOpen = isOpen => {
  setLSItem(LS_KEYS.apiExplorerEndpointSectionIsOpen, isOpen);
};

export const getEndPointSectionIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = getLSItem(LS_KEYS.apiExplorerEndpointSectionIsOpen);

  return isOpen ? isOpen === 'true' : defaultIsOpen;
};

export const setHeadersSectionIsOpen = isOpen => {
  setLSItem(LS_KEYS.apiExplorerHeaderSectionIsOpen, isOpen);
};

export const getHeadersSectionIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = getLSItem(LS_KEYS.apiExplorerHeaderSectionIsOpen);

  return isOpen ? isOpen === 'true' : defaultIsOpen;
};

export const getAdminSecret = () => {
  let adminSecret = null;
  if (globals.consoleMode === SERVER_CONSOLE_MODE && globals.isAdminSecretSet) {
    const adminSecretFromLS = loadAdminSecretState();
    const adminSecretInGlobals = globals.adminSecret;

    adminSecret = adminSecretFromLS || adminSecretInGlobals;
  } else {
    adminSecret = globals.adminSecret;
  }

  return adminSecret;
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

export const persistGraphiQLHeaders = headers => {
  // filter empty headers
  const validHeaders = headers.filter(h => h.key);

  // remove admin-secret value
  const maskedHeaders = validHeaders.map(h => {
    const maskedHeader = { ...h };

    if (h.key.toLowerCase() === ADMIN_SECRET_HEADER_KEY) {
      maskedHeader.value = 'xxx';
    }

    return maskedHeader;
  });

  setLSItem(
    LS_KEYS.apiExplorerConsoleGraphQLHeaders,
    JSON.stringify(maskedHeaders)
  );
};

export const getPersistedGraphiQLHeaders = () => {
  const headersString = getLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders);

  let headers = null;
  if (headersString) {
    try {
      headers = JSON.parse(headersString);

      // add admin-secret value
      headers = headers.map(h => {
        const unmaskedHeader = { ...h };

        if (h.key.toLowerCase() === ADMIN_SECRET_HEADER_KEY) {
          unmaskedHeader.value = getAdminSecret();
        }

        return unmaskedHeader;
      });
    } catch (_) {
      console.error('Failed parsing headers from local storage');
    }
  }

  return headers;
};

export const getDefaultGraphiqlHeaders = () => {
  const headers = [];

  headers.push({
    key: 'content-type',
    value: 'application/json',
    isActive: true,
    isNewHeader: false,
    isDisabled: false,
  });

  return headers;
};

export const parseAuthHeader = header => {
  let isAuthHeader = false;
  let token = null;

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

export const persistGraphiQLMode = mode => {
  setLSItem(LS_KEYS.apiExplorerGraphiqlMode, mode);
};

export const getPersistedGraphiQLMode = () => {
  return getLSItem(LS_KEYS.apiExplorerGraphiqlMode) || 'graphql';
};
