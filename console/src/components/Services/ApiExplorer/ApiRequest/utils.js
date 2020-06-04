import globals from '../../../../Globals';
import { loadAdminSecretState } from '../../../AppState';
import {
  ADMIN_SECRET_HEADER_KEY,
  SERVER_CONSOLE_MODE,
} from '../../../../constants';

export const setEndPointSectionIsOpen = isOpen => {
  window.localStorage.setItem('ApiExplorer:EndpointSectionIsOpen', isOpen);
};

export const getEndPointSectionIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = window.localStorage.getItem(
    'ApiExplorer:EndpointSectionIsOpen'
  );

  return isOpen ? isOpen === 'true' : defaultIsOpen;
};

export const setHeadersSectionIsOpen = isOpen => {
  window.localStorage.setItem('ApiExplorer:HeadersSectionIsOpen', isOpen);
};

export const getHeadersSectionIsOpen = () => {
  const defaultIsOpen = true;

  const isOpen = window.localStorage.getItem(
    'ApiExplorer:HeadersSectionIsOpen'
  );

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

const LS_API_EXPLORER_ADMIN_SECRET_HEADER_WAS_ADDED =
  'ApiExplorer:AdminSecretHeaderWasAdded';

export const persistAdminSecretHeaderWasAdded = () => {
  window.localStorage.setItem(
    LS_API_EXPLORER_ADMIN_SECRET_HEADER_WAS_ADDED,
    'true'
  );
};

export const removePersistedAdminSecretHeaderWasAdded = () => {
  window.localStorage.removeItem(LS_API_EXPLORER_ADMIN_SECRET_HEADER_WAS_ADDED);
};

export const getPersistedAdminSecretHeaderWasAdded = () => {
  const lsValue = window.localStorage.getItem(
    LS_API_EXPLORER_ADMIN_SECRET_HEADER_WAS_ADDED
  );

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

  window.localStorage.setItem(
    'HASURA_CONSOLE_GRAPHIQL_HEADERS',
    JSON.stringify(maskedHeaders)
  );
};

export const getPersistedGraphiQLHeaders = () => {
  const headersString = window.localStorage.getItem(
    'HASURA_CONSOLE_GRAPHIQL_HEADERS'
  );

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

export const persistGraphiQLMode = (mode) => {
  window.localStorage.setItem('ApiExplorer:GraphiQLMode', mode);
};

export const getPersistedGraphiQLMode = () => {
  return window.localStorage.getItem('ApiExplorer:GraphiQLMode');
};
