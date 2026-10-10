import {
  getWebsocketProtocol,
  replaceURLHttpToWs,
  stripTrailingSlash,
} from '@hasura/shared/utils';
import { EnvVars } from '@hasura/shared/types';

export const getEndpoints = (globals: EnvVars, baseUrl: string) => {
  const hasuraCliServerUrl = `${globals.apiHost}:${globals.apiPort}`;
  const graphQLUrl = `${baseUrl}/v1/graphql`;
  const relayURL = `${baseUrl}/v1/relay`;
  const luxDataHost = globals.luxDataHost
    ? stripTrailingSlash(globals.luxDataHost)
    : '';
  const endpoints = {
    graphQLUrl,
    relayURL,
    baseUrl,
    graphQLWebSocketURL: replaceURLHttpToWs(graphQLUrl),
    relayWebSocketURL: replaceURLHttpToWs(relayURL),
    serverConfig: `${baseUrl}/v1alpha1/config`,
    query: `${baseUrl}/v2/query`,
    entitlement: `${baseUrl}/v1/entitlement`,
    license: `${baseUrl}/v1/entitlement/license`,
    metadata: `${baseUrl}/v1/metadata`,
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
    luxDataGraphql: `${window.location.protocol}//${luxDataHost}/v1/graphql`,
    luxDataGraphqlWs: `${getWebsocketProtocol(window.location.protocol)}//${
      luxDataHost
    }/v1/graphql`,
    prometheusUrl: `${baseUrl}/v1/metrics`,
    registerEETrial: `https://licensing.pro.hasura.io/v1/graphql`,
    schemaRegistry: `${window.location.protocol}//${globals.schemaRegistryHost}/v1/graphql`,
    exportOpenApi: `${baseUrl}/api/swagger/json`,
  };

  return endpoints;
};

export type Endpoints = ReturnType<typeof getEndpoints>;
