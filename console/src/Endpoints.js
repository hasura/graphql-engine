import globals from './Globals';

const baseUrl = globals.dataApiUrl;
const hasuractlApiHost = globals.apiHost;
const hasuractlApiPort = globals.apiPort;

const hasuractlUrl = hasuractlApiHost + ':' + hasuractlApiPort;

const Endpoints = {
  getSchema: `${baseUrl}/v1/query`,
  graphQLUrl: `${baseUrl}/v1alpha1/graphql`,
  schemaChange: `${baseUrl}/v1/query`,
  query: `${baseUrl}/v1/query`,
  rawSQL: `${baseUrl}/v1/query`,
  version: `${baseUrl}/v1/version`,
  updateCheck: 'https://releases.hasura.io/graphql-engine',
  hasuractlMigrate: `${hasuractlUrl}/apis/migrate`,
  hasuractlMetadata: `${hasuractlUrl}/apis/metadata`,
  hasuractlMigrateSettings: `${hasuractlUrl}/apis/migrate/settings`,
};

const globalCookiePolicy = 'omit';

export default Endpoints;
export { globalCookiePolicy, baseUrl, hasuractlUrl };
