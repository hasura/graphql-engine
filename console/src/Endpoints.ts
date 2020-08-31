import globals from './Globals';

const baseUrl = globals.dataApiUrl;
const hasuractlApiHost = globals.apiHost;
const hasuractlApiPort = globals.apiPort;

const hasuractlUrl = `${hasuractlApiHost}:${hasuractlApiPort}`;

const Endpoints = {
  getSchema: `${baseUrl}/v1/query`,
  serverConfig: `${baseUrl}/v1alpha1/config`,
  graphQLUrl: `${baseUrl}/v1/graphql`,
  relayURL: `${baseUrl}/v1beta1/relay`,
  schemaChange: `${baseUrl}/v1/query`,
  query: `${baseUrl}/v1/query`,
  rawSQL: `${baseUrl}/v1/query`,
  version: `${baseUrl}/v1/version`,
  updateCheck: 'https://releases.hasura.io/graphql-engine',
  hasuractlMigrate: `${hasuractlUrl}/apis/migrate`,
  hasuractlMetadata: `${hasuractlUrl}/apis/metadata`,
  hasuractlMigrateSettings: `${hasuractlUrl}/apis/migrate/settings`,
  telemetryServer: 'wss://telemetry.hasura.io/v1/ws',
  consoleNotificationsStg: 'https://data.hasura-stg.hasura-app.io/v1/query',
  consoleNotificationsProd: 'https://data.hasura.io/v1/query',
};

const globalCookiePolicy = 'same-origin';

export default Endpoints;
export { globalCookiePolicy, baseUrl, hasuractlUrl };
