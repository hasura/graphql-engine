export const baseUrl = Cypress.config('baseUrl');
export const getRemoteSchemaName = (i, schemaName) =>
  `test-remote-schema-${schemaName}-${i}`;
export const getRemoteGraphQLURL = () =>
  'https://realtime-poll.demo.hasura.app/v1/graphql';
export const getRemoteGraphQLURLFromEnv = () => 'GRAPHQL_URL';
export const getInvalidRemoteSchemaUrl = () => 'http://httpbin.org/post';
export const getHeaderAccessKey = i => `ACCESS_KEY-${i}`;
export const getHeaderAccessKeyValue = () => 'b94264abx98';

export const getElementFromAlias = alias => `[data-test=${alias}]`;
export const makeDataAPIUrl = dataApiUrl => `${dataApiUrl}/v1/query`;
export const makeDataAPIOptions = (dataApiUrl, key, body) => ({
  method: 'POST',
  url: makeDataAPIUrl(dataApiUrl),
  headers: {
    'x-hasura-admin-secret': key,
  },
  body,
  failOnStatusCode: false,
});
