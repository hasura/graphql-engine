import { CLI_CONSOLE_MODE } from '../constants';

const serverEnvVars = `
  dataApiUrl: '${process.env.DATA_API_URL}',
  isAdminSecretSet: '${process.env.IS_ADMIN_SECRET_SET}',
  consoleMode: '${process.env.CONSOLE_MODE}',
  nodeEnv: '${process.env.NODE_ENV}',
  serverVersion: '${process.env.SERVER_VERSION}',
  urlPrefix: '${process.env.URL_PREFIX}',
  consolePath: '${process.env.CONSOLE_PATH}',
  enableTelemetry: ${process.env.ENABLE_TELEMETRY},
  assetsPath: '${process.env.ASSETS_PATH}',
  assetsVersion: '${process.env.ASSETS_VERSION}',
  cdnAssets: ${process.env.CDN_ASSETS},
  herokuOAuthClientId: '${process.env.HEROKU_OAUTH_CLIENT_ID || ''}',
  tenantID: '${process.env.HASURA_CLOUD_TENANT_ID || ''}',
  projectID: '${process.env.HASURA_CLOUD_PROJECT_ID || ''}',
  cloudRootDomain: '${process.env.HASURA_CLOUD_ROOT_DOMAIN}',
  consoleType: '${process.env.HASURA_CONSOLE_TYPE}',
`;

const cliEnvVars = `
  apiPort: '${process.env.API_PORT}',
  apiHost: '${process.env.API_HOST}',
  dataApiUrl: '${process.env.DATA_API_URL}',
  adminSecret: '${process.env.ADMIN_SECRET}',
  consoleMode: '${process.env.CONSOLE_MODE}',
  nodeEnv: '${process.env.NODE_ENV}',
  enableTelemetry: ${process.env.ENABLE_TELEMETRY},
  assetsPath: '${process.env.ASSETS_PATH}',
  assetsVersion: '${process.env.ASSETS_VERSION}',
  serverVersion: '${process.env.SERVER_VERSION}',
  cdnAssets: ${process.env.CDN_ASSETS},
  herokuOAuthClientId: '${process.env.HEROKU_OAUTH_CLIENT_ID || ''}',
  tenantID: '${process.env.HASURA_CLOUD_TENANT_ID || ''}',
  projectID: '${process.env.HASURA_CLOUD_PROJECT_ID || ''}',
  cloudRootDomain: '${process.env.HASURA_CLOUD_ROOT_DOMAIN}'
`;

const envVars =
  process.env.CONSOLE_MODE === CLI_CONSOLE_MODE ? cliEnvVars : serverEnvVars;

export const env = `
  window.__env = {${envVars}};
`;
