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

export const setGraphiQLHeadersInLocalStorage = headers => {
  // filter empty headers
  const validHeaders = headers.filter(h => h.key);

  // remove admin-secret value
  const maskedHeaders = validHeaders.map(h => {
    if (h.key.toLowerCase() === ADMIN_SECRET_HEADER_KEY) {
      h.value = 'xxx';
    }

    return h;
  });

  window.localStorage.setItem(
    'HASURA_CONSOLE_GRAPHIQL_HEADERS',
    JSON.stringify(maskedHeaders)
  );
};

export const getAdminSecret = () => {
  let adminSecret = null;
  if (globals.consoleMode === SERVER_CONSOLE_MODE && globals.isAdminSecretSet) {
    const adminSecretFromLS = loadAdminSecretState();
    const adminSecretInRedux = ''; // TODO
    // const adminSecretInRedux = getState().main.adminSecret;

    adminSecret = adminSecretFromLS || adminSecretInRedux;
  } else {
    adminSecret = globals.adminSecret;
  }

  return adminSecret;
};

export const getGraphiQLHeadersFromLocalStorage = () => {
  const headersString = window.localStorage.getItem(
    'HASURA_CONSOLE_GRAPHIQL_HEADERS'
  );

  let headers = null;
  if (headersString) {
    try {
      headers = JSON.parse(headersString);

      // add admin-secret value
      headers = headers.map(h => {
        if (h.key.toLowerCase() === ADMIN_SECRET_HEADER_KEY) {
          h.value = getAdminSecret();
        }

        return h;
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

  const adminSecret = getAdminSecret();
  if (adminSecret) {
    headers.push({
      key: ADMIN_SECRET_HEADER_KEY,
      value: adminSecret,
      isActive: true,
      isNewHeader: false,
      isDisabled: false,
    });
  }

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
