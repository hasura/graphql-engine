import globals from '../Globals';

export const setLSItem = (key: string, data: string) => {
  window.localStorage.setItem(key, data);
};

export const getLSItem = (key: string) => {
  if (!key) {
    return null;
  }

  return window.localStorage.getItem(key);
};

export const getParsedLSItem = (key: string, defaultVal: any = null) => {
  const value = getLSItem(key);

  if (!value) {
    return defaultVal;
  }

  try {
    const jsonValue = JSON.parse(value);

    return jsonValue || defaultVal;
  } catch {
    return defaultVal;
  }
};

export const removeLSItem = (key: string) => {
  const value = getLSItem(key);

  if (!value) {
    return null;
  }

  window.localStorage.removeItem(key);
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

export const listLSKeys = () => {
  return Object.keys(window.localStorage);
};

export const LS_KEYS = {
  consoleAdminSecret: 'console:adminSecret',
  consoleLocalInfo: `console:localInfo:${globals.dataApiUrl}`,
  versionUpdateCheckLastClosed: 'console:versionUpdateCheckLastClosed',
  loveConsent: 'console:loveIcon',
  proClick: 'console:pro',
  derivedActions: 'actions:derivedActions',
  apiExplorerEndpointSectionIsOpen: 'apiExplorer:endpointSectionIsOpen',
  apiExplorerGraphiqlMode: 'apiExplorer:graphiQLMode',
  apiExplorerHeaderSectionIsOpen: 'apiExplorer:headersSectionIsOpen',
  apiExplorerAdminSecretWasAdded: 'apiExplorer:adminSecretHeaderWasAdded',
  apiExplorerConsoleGraphQLHeaders: 'apiExplorer:graphiqlHeaders',
  oneGraphExplorerWidth: 'graphiql:explorerWidth',
  oneGraphExplorerOpen: 'graphiql:explorerOpen',
  oneGraphExplorerCodeExporterOpen: 'graphiql:codeExporterOpen',
  graphiqlQuery: 'graphiql:query',
  rawSQLKey: 'rawSql:sql',
  rawSqlStatementTimeout: 'rawSql:rawSqlStatementTimeout',
  dataColumnsCollapsedKey: 'data:collapsed',
  dataPageSizeKey: 'data:pageSize',
  dataColumnsOrderKey: 'data:order',
};

export const clearGraphiqlLS = () => {
  Object.values(LS_KEYS).forEach(lsKey => {
    if (lsKey.startsWith('graphiql:')) {
      window.localStorage.removeItem(lsKey);
    }
  });
};
