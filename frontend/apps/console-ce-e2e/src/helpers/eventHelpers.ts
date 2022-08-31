import { ADMIN_SECRET_HEADER_KEY } from './constants';

export const baseUrl = Cypress.config('baseUrl');
export const queryTypes = ['insert', 'update', 'delete'];

export const getTriggerName = (i: number, testName = '') =>
  `Apic_test_trigger_${testName}_${i}`;

export const getTableName = (i: number, testName = '') =>
  `Apic_test_table_${testName}_${i}`;

export const getWebhookURL = () => 'http://httpbin.org/post';
export const getNoOfRetries = () => '5';
export const getIntervalSeconds = () => '10';
export const getTimeoutSeconds = () => '25';

export const getElementFromAlias = (alias: string) => `[data-test=${alias}]`;
export const makeDataAPIUrl = (dataApiUrl: string) => `${dataApiUrl}/v1/query`;

interface APIPayload {
  [key: string]: any;
}
export const makeDataAPIOptions = (
  dataApiUrl: string,
  key: string,
  body: APIPayload
) => ({
  method: 'POST',
  url: makeDataAPIUrl(dataApiUrl),
  headers: {
    [ADMIN_SECRET_HEADER_KEY]: key,
  },
  body,
  failOnStatusCode: false,
});
