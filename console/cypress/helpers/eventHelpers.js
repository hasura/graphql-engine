export const baseUrl = Cypress.config('baseUrl');
export const queryTypes = ['insert', 'update', 'delete'];
export const getTriggerName = (i, testName = '') =>
  `Apic_test_trigger_${testName}_${i}`;
export const getTableName = (i, testName = '') =>
  `Apic_test_table_${testName}_${i}`;
export const getWebhookURL = () => 'http://httpbin.org/post';
export const getNoOfRetries = () => '5';
export const getIntervalSeconds = () => '10';
export const getTimeoutSeconds = () => '25';
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
