import globals from '../Globals';

/*
In Hasura cloud, console local storage functions have been monkey-patched to separate out local storage keys
based on project ids. i.e. Hasura cloud dashboard and the different project consoles and do not share the same local
storage keys.

See monkeypatch code: https://github.com/hasura/lux/blob/0845a55/services/cloud/team_console/index.html#L24-L103

To share local storage keys between different project consoles or between cloud dashboard and console, the
localstorage key needs to be added to the `GLOBAL_LS_KEYS` variable in the above file.
*/

/* IMPORTANT: for behaviour on Cloud console, see note at start of file */
export const setLSItem = (key: string, data: string) => {
  window.localStorage.setItem(key, data);
};

/* IMPORTANT: for behaviour on Cloud console, see note at start of file */
export const getLSItem = (key: string) => {
  if (!key) {
    return null;
  }

  return window.localStorage.getItem(key);
};

/* IMPORTANT: for behaviour on Cloud console, see note at start of file */
export const removeLSItem = (key: string) => {
  const value = getLSItem(key);

  if (!value) {
    return null;
  }

  window.localStorage.removeItem(key);
  return true;
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
    removeLSItem(key);
    return null;
  }

  return item.value;
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

export const listLSKeys = () => {
  return Object.keys(window.localStorage);
};

export const LS_KEYS = {
  apiExplorerAdminSecretWasAdded: 'apiExplorer:adminSecretHeaderWasAdded',
  apiExplorerConsoleGraphQLHeaders: 'apiExplorer:graphiqlHeaders',
  apiExplorerEndpointSectionIsOpen: 'apiExplorer:endpointSectionIsOpen',
  apiExplorerGraphiqlMode: 'apiExplorer:graphiQLMode',
  apiExplorerHeaderSectionIsOpen: 'apiExplorer:headersSectionIsOpen',
  consoleAdminSecret: 'console:adminSecret',
  consoleLocalInfo: `console:localInfo:${globals.dataApiUrl}`,
  dataColumnsCollapsedKey: 'data:collapsed',
  dataColumnsOrderKey: 'data:order',
  dataPageSizeKey: 'data:pageSize',
  derivedActions: 'actions:derivedActions',
  graphiqlQuery: 'graphiql:query',
  graphiqlVariables: 'graphiql:variables',
  graphiqlVariablesHeight: 'graphiql:variableEditorHeight',
  loveConsent: 'console:loveIcon',
  oneGraphExplorerCodeExporterOpen: 'graphiql:codeExporterOpen',
  oneGraphExplorerOpen: 'graphiql:explorerOpen',
  oneGraphExplorerWidth: 'graphiql:explorerWidth',
  proClick: 'console:pro',
  rawSQLKey: 'rawSql:sql',
  rawSqlStatementTimeout: 'rawSql:rawSqlStatementTimeout',
  showConsoleOnboarding: 'console:showConsoleOnboarding',
  versionUpdateCheckLastClosed: 'console:versionUpdateCheckLastClosed',
  vpcBannerLastDismissed: 'console:vpcBannerLastDismissed',
  webhookTransformEnvVars: 'console:webhookTransformEnvVars',
  featureFlag: 'console:featureFlag',
  permissionConfirmationModalStatus:
    'console:permissionConfirmationModalStatus',
  neonCallbackSearch: 'neon:authCallbackSearch',
  slackCallbackSearch: 'slack:authCallbackSearch',
  herokuCallbackSearch: 'HEROKU_CALLBACK_SEARCH',
  consolePersonalAccessToken: 'PERSONAL_ACCESS_TOKEN',
  notificationsData: 'notifications:data',
  notificationsLastSeen: 'notifications:lastSeen',
  authState: 'AUTH_STATE',
  skipOnboarding: 'SKIP_CLOUD_ONBOARDING',
  lastViewedSchemaChange: 'LAST_VIEWED_SCHEMA_CHANGE',
};

export const clearGraphiqlLS = () => {
  Object.values(LS_KEYS).forEach(lsKey => {
    if (lsKey.startsWith('graphiql:')) {
      removeLSItem(lsKey);
    }
  });
};
