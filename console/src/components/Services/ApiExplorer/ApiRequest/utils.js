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
  window.localStorage.setItem('HASURA_CONSOLE_GRAPHIQL_HEADERS', headers);
};

export const getGraphiQLHeadersFromLocalStorage = () => {
  return window.localStorage.getItem('HASURA_CONSOLE_GRAPHIQL_HEADERS');
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
