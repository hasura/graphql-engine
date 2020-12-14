import globals from '../../../Globals';

export const getLocalStorageItem = key => {
  return window.localStorage.getItem(key);
};

export const setLocalStorageItem = (key, value) => {
  window.localStorage.setItem(key, value);
};

/** Local storage keys **/

export const LS_VERSION_UPDATE_CHECK_LAST_CLOSED =
  'versionUpdateCheck: lastClosed';

export const LS_RAW_SQL_STATEMENT_TIMEOUT = 'rawSql:rawSqlStatementTimeout';

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
