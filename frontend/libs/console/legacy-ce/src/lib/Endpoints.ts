import { getWebsocketProtocol } from './helpers/protocol';
import consoleGlobals from './Globals';

export const globalCookiePolicy = 'same-origin';

export const baseUrl = consoleGlobals.dataApiUrl;

export const getEndpoints = (globals: typeof consoleGlobals) => {
  const hasuraCliServerUrl = `${globals.apiHost}:${globals.apiPort}`;

  const endpoints = {
    serverConfig: `${baseUrl}/v1alpha1/config`,
    graphQLUrl: `${baseUrl}/v1/graphql`,
    relayURL: `${baseUrl}/v1beta1/relay`,
    query: `${baseUrl}/v2/query`,
    entitlement: `${baseUrl}/v1/entitlement`,
    license: `${baseUrl}/v1/entitlement/license`,
    metadata: `${baseUrl}/v1/metadata`,
    // metadata: `${baseUrl}/v1/query`,
    queryV2: `${baseUrl}/v2/query`,
    version: `${baseUrl}/v1/version`,
    updateCheck: 'https://releases.hasura.io/graphql-engine',
    hasuraCliServerMigrate: `${hasuraCliServerUrl}/apis/migrate`,
    hasuraCliServerMetadata: `${hasuraCliServerUrl}/apis/metadata`,
    hasuraCliServerMigrateSettings: `${hasuraCliServerUrl}/apis/migrate/settings`,
    telemetryServer: 'wss://telemetry.hasura.io/v1/ws',
    consoleNotificationsStg:
      'https://notifications.hasura-stg.hasura-app.io/v1/graphql',
    consoleNotificationsProd: 'https://notifications.hasura.io/v1/graphql',
    luxDataGraphql: `${window.location.protocol}//${globals.luxDataHost}/v1/graphql`,
    luxDataGraphqlWs: `${getWebsocketProtocol(window.location.protocol)}//${
      globals.luxDataHost
    }/v1/graphql`,
    prometheusUrl: `${baseUrl}/v1/metrics`,
    registerEETrial: `https://licensing.pro.hasura.io/v1/graphql`,
    schemaRegistry: `${window.location.protocol}//${globals.schemaRegistryHost}/v1/graphql`,
    // registerEETrial: `http://licensing.lux-dev.hasura.me/v1/graphql`,
    exportOpenApi: 'api/swagger/json',
  };

  return endpoints;
};

const endpoints = getEndpoints(consoleGlobals);
export default endpoints;
