import globals from './Globals';

const baseUrl = globals.dataApiUrl;
const hasuractlApiHost = globals.apiHost;
const hasuractlApiPort = globals.apiPort;

const hasuractlUrl = `${hasuractlApiHost}:${hasuractlApiPort}`;

const Endpoints = {
  serverConfig: `${baseUrl}/v1alpha1/config`,
  graphQLUrl: `${baseUrl}/v1/graphql`,
  relayURL: `${baseUrl}/v1beta1/relay`,
  query: `${baseUrl}/v2/query`,
  metadata: `${baseUrl}/v1/metadata`,
  // metadata: `${baseUrl}/v1/query`,
  queryV2: `${baseUrl}/v2/query`,
  version: `${baseUrl}/v1/version`,
  updateCheck: 'https://releases.hasura.io/graphql-engine',
  hasuractlMigrate: `${hasuractlUrl}/apis/migrate`,
  hasuractlMetadata: `${hasuractlUrl}/apis/metadata`,
  hasuractlMigrateSettings: `${hasuractlUrl}/apis/migrate/settings`,
  telemetryServer: 'wss://telemetry.hasura.io/v1/ws',
  consoleNotificationsStg:
    'https://notifications.hasura-stg.hasura-app.io/v1/graphql',
  consoleNotificationsProd: 'https://notifications.hasura.io/v1/graphql',
  luxDataGraphql: globals.luxDataHost
    ? `${window.location.protocol}//${globals.luxDataHost}/v1/graphql`
    : `${globals.cloudDataApiUrl}/v1/graphql`,
};

const globalCookiePolicy = 'same-origin';

export default Endpoints;
export { globalCookiePolicy, baseUrl, hasuractlUrl };
