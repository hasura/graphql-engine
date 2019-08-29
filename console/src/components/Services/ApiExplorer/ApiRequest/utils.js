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
  // filter empty & admin secret headers
  const filteredHeaders = headers.filter(
    h => h.key && h.key.toLowerCase() !== 'x-hasura-admin-secret'
  );

  window.localStorage.setItem(
    'HASURA_CONSOLE_GRAPHIQL_HEADERS',
    JSON.stringify(filteredHeaders)
  );
};

export const getGraphiQLHeadersFromLocalStorage = () => {
  const headersString = window.localStorage.getItem(
    'HASURA_CONSOLE_GRAPHIQL_HEADERS'
  );

  let headers = null;
  if (headersString) {
    try {
      headers = JSON.parse(headersString);
    } catch (_) {
      console.error('Failed parsing headers from local storage');
    }
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
