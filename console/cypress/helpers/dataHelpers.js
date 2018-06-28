export const baseUrl = Cypress.env('BASE_URL');
export const dataTypes = [
  'serial',
  'bigserial',
  'integer',
  'bigint',
  'text',
  'numeric',
  'date',
  'timestamptz',
  'timetz',
  'boolean',
];
export const typeDefaults = {
  integer: '5555',
  bigint: '5555555555',
  text: 'test-text',
  numeric: '0.55555',
  date: 'now()',
  timestamptz: 'now()',
  timetz: 'now()',
  boolean: 'false',
};
export const queryTypes = ['insert', 'select', 'update', 'delete'];
export const getColName = i => `apic_test_column_${i}`;
export const getTableName = i => `apic_test_table_${i}`;
export const getElementFromAlias = alias => `[data-test=${alias}]`;
export const makeDataAPIUrl = dataApiUrl => `${dataApiUrl}/v1/query`;
export const makeDataAPIOptions = (dataApiUrl, key, body) => ({
  method: 'POST',
  url: makeDataAPIUrl(dataApiUrl),
  headers: {
    'x-hasura-access-key': key,
  },
  body,
  failOnStatusCode: false,
});
