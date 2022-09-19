const serverEnvVars = `
  dataApiUrl: '${process.env.DATA_API_URL}',
  adminSecret: '${process.env.ADMIN_SECRET}',
  isAdminSecretSet: ${process.env.IS_ADMIN_SECRET_SET},
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
  neonOAuthClientId: '${process.env.NEON_OAUTH_CLIENT_ID}',
  neonRootDomain: '${process.env.NEON_ROOT_DOMAIN}',
  tenantID: '${process.env.HASURA_CLOUD_TENANT_ID || ''}',
  projectID: '${process.env.HASURA_CLOUD_PROJECT_ID || ''}',
  cloudRootDomain: '${process.env.HASURA_CLOUD_ROOT_DOMAIN}',
  consoleType: '${process.env.HASURA_CONSOLE_TYPE}',
  allowedLuxFeatures: ${process.env.ALLOWED_LUX_FEATURES},
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
  tenantID: '${process.env.HASURA_CLOUD_TENANT_ID || ''}',
  projectID: '${process.env.HASURA_CLOUD_PROJECT_ID || ''}',
  cloudRootDomain: '${process.env.HASURA_CLOUD_ROOT_DOMAIN}',
  consoleSentryDsn: '${process.env.HASURA_CONSOLE_SENTRY_DSN}'
`;

const envVars = process.env.CONSOLE_MODE === 'cli' ? cliEnvVars : serverEnvVars;

module.exports.env = `
  window.__env = {${envVars}};
`;
