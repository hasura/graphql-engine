import globals from './Globals';

import {
  Endpoints as endpointOSS,
  EndpointNamedExps,
} from '@hasura/console-legacy-ce';

const metricsBaseUrl = globals.metricsApiUrl;
export const { globalCookiePolicy, baseUrl, hasuractlUrl } = EndpointNamedExps;

const Endpoints = {
  ...endpointOSS,
  metricsGraphQLUrl: `${metricsBaseUrl}/v1/graphql`,
  metadata: `${baseUrl}/v1/metadata`,
  queryV1: `${baseUrl}/v1/query`,
  health: `${baseUrl}/healthz`,
};

export default Endpoints;
