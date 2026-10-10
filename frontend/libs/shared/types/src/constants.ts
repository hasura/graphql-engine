export const SERVER_CONSOLE_MODE = 'server';
export const CLI_CONSOLE_MODE = 'cli';
export const ADMIN_SECRET_HEADER_KEY = 'x-hasura-admin-secret';
export const HASURA_COLLABORATOR_TOKEN = 'hasura-collaborator-token';
export const HASURA_SSO_TOKEN = 'hasura-sso-token';
export const HASURA_CLIENT_NAME = 'hasura-client-name';
export const CLIENT_NAME_HEADER_VALUE = 'hasura-console';
export const CONTENT_TYPE_HEADER = 'Content-Type';

export const LOGIN_PATH = '/login';
export const METADATA_STATUS_PATH = `/settings/metadata-status?is_redirected=true`;
export const OAUTH_CALLBACK_URL = '/oauth2/callback';

export const EVENTS_SERVICE_HEADING = 'Events';
export const ADHOC_EVENTS_HEADING = 'One-off Scheduled Events';
export const CRON_EVENTS_HEADING = 'Cron Triggers';
export const CRON_TRIGGER = 'Cron Trigger';
export const EVENT_TRIGGER = 'Event Trigger';
export const DATA_EVENTS_HEADING = 'Event Triggers';

export const LS_KEYS = {
  apiExplorerAdminSecretWasAdded: 'apiExplorer:adminSecretHeaderWasAdded',
  apiExplorerConsoleGraphQLHeaders: 'apiExplorer:graphiqlHeaders',
  apiExplorerGraphiqlMode: 'apiExplorer:graphiQLMode',
  apiExplorerHeaderSectionIsOpen: 'apiExplorer:headersSectionIsOpen',
  consoleAuthState: 'console:authState',
  consoleOAuthLoginSessionState: 'console:oauthLoginSessionState',
  dataColumnsCollapsedKey: 'data:collapsed',
  dataColumnsOrderKey: 'data:order',
  dataPageSizeKey: 'data:pageSize',
  derivedActions: 'actions:derivedActions',
  graphiqlQuery: 'graphiql:query',
  graphiqlVariables: 'graphiql:variables',
  graphiqlVariablesHeight: 'graphiql:variableEditorHeight',
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
  notificationsLastSeen: 'notifications:lastSeen',
  skipOnboarding: 'SKIP_CLOUD_ONBOARDING',
  lastViewedSchemaChange: 'LAST_VIEWED_SCHEMA_CHANGE',
};
