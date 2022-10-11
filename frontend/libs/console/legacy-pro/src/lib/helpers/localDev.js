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
  consoleId: '${process.env.HASURA_CLIENT_ID}',
  hasuraOAuthUrl: '${process.env.HASURA_OAUTH_URL}',
  hasuraOAuthScopes: '${process.env.HASURA_OAUTH_SCOPES}',
  cloudRootDomain: '${process.env.HASURA_CLOUD_ROOT_DOMAIN}',
  consoleType: '${process.env.HASURA_CONSOLE_TYPE}',
  consoleSentryDsn: '${process.env.HASURA_CONSOLE_SENTRY_DSN}'
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
  projectId: '${process.env.PROJECT_ID}',
  projectName: '${process.env.PROJECT_NAME}',
  hasuraMetricsUrl: '${process.env.HASURA_METRICS_URL}',
  pro: ${process.env.IS_PRO},
  cloudRootDomain: '${process.env.HASURA_CLOUD_ROOT_DOMAIN}',
  consoleSentryDsn: '${process.env.HASURA_CONSOLE_SENTRY_DSN}'
`;

const envVars = process.env.CONSOLE_MODE === 'cli' ? cliEnvVars : serverEnvVars;

module.exports.env = `
  window.__env = {${envVars}};
`;
