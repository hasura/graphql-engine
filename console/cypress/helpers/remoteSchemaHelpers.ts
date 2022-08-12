import { ADMIN_SECRET_HEADER_KEY } from './constants';

export const baseUrl = Cypress.config('baseUrl');

export const getRemoteSchemaName = (i: number, schemaName: string) =>
  `test-remote-schema-${schemaName}-${i}`;

export const getRemoteGraphQLURL = () =>
  'https://hasura-console-test.herokuapp.com/v1/graphql/';

export const getRemoteGraphQLURLFromEnv = () => 'GRAPHQL_URL';

export const getInvalidRemoteSchemaUrl = () => 'http://httpbin.org/post';

export const getHeaderAccessKey = (i: string) => `ACCESS_KEY-${i}`;

export const getHeaderAccessKeyValue = () => 'b94264abx98';

export const getElementFromAlias = (alias: string) => `[data-test=${alias}]`;

export const makeDataAPIUrl = (dataApiUrl: string) => `${dataApiUrl}/v1/query`;

export const makeDataAPIOptions = (
  dataApiUrl: string,
  key: string,
  body: { [key: string]: any }
) => ({
  method: 'POST',
  url: makeDataAPIUrl(dataApiUrl),
  headers: {
    [ADMIN_SECRET_HEADER_KEY]: key,
  },
  body,
  failOnStatusCode: false,
});

export const getRemoteSchemaRoleName = (i: number, roleName: string) =>
  `test-role-${roleName}-${i}`;
