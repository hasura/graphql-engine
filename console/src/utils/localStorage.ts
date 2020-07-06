import globals from '../Globals';

export const setLSItem = (key: string, data: string) => {
  window.localStorage.setItem(key, data);
};

export const getLSItem = (key: string) => {
  return window.localStorage.getItem(key);
};

export const removeLSItem = (key: string) => {
  if (getLSItem(key)) {
    window.localStorage.removeItem(key);
  }
};

type expiryValue = {
  value: string;
  expiry: number;
};

export const setLSItemWithExpiry = (key: string, data: string, ttl: number) => {
  const now = new Date();

  const item: expiryValue = {
    value: data,
    expiry: now.getTime() + ttl,
  };

  setLSItem(key, JSON.stringify(item));
};

export const getItemWithExpiry = (key: string) => {
  const lsValue = getLSItem(key);
  if (!lsValue) {
    return null;
  }
  const item: expiryValue = JSON.parse(lsValue);
  const now = new Date();

  if (now.getTime() > item.expiry) {
    window.localStorage.removeItem(key);
    return null;
  }

  return item.value;
};

// NOTE: use with extreme caution
export const clearLS = () => {
  window.localStorage.clear();
};

export const listLSKeys = () => {
  return Object.keys(window.localStorage);
};

// This is the list of the localStorage keys that are being used.
export const lsKeys = {
  consoleAdminSecret: 'CONSOLE_ADMIN_SECRET',
  consoleLocalInfo: `CONSOLE_LOCAL_INFO:${globals.dataApiUrl}`,
  versionUpdateCheckLastClosed: 'versionUpdateCheck: lastClosed',
  rawSqlStatementTimeout: 'rawSql:rawSqlStatementTimeout',
  loveConsent: 'console:loveIcon',
  proClick: 'console:pro',
  derivedActions: 'actions:derivedActions',
  graphiqlQuery: 'graphiql:query',
  apiExplorerEndpointSectionIsOpen: 'ApiExplorer:EndpointSectionIsOpen',
  apiExplorerGraphiqlMode: 'ApiExplorer:GraphiQLMode',
  apiExplorerHeaderSectionIsOpen: 'ApiExplorer:HeadersSectionIsOpen',
  apiExplorerAdminSecretWasAdded: 'ApiExplorer:AdminSecretHeaderWasAdded',
  apiExplorerConsoleGraphQLHeaders: 'HASURA_CONSOLE_GRAPHIQL_HEADERS',
  oneGraphExplorerWidth: 'graphiql:explorerWidth',
  oneGraphExplorerOpen: 'graphiql:explorerOpen',
  oneGraphExplorerCodeExporterOpen: 'graphiql:codeExporterOpen',
  rawSQLKey: 'rawSql:sql',
  dataColumnsCollapsedKey: 'data:collapsed',
  dataPageSizeKey: 'data:pageSize',
};
