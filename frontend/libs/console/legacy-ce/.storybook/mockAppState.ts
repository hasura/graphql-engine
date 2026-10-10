import { AppState, getEndpoints } from '../../../shared/context/src';
import { EnvVars } from '@hasura/shared/types';

const baseUrl = 'http://localhost:8080';

export const mockEnvVars: EnvVars = {
  nodeEnv: 'development',
  serverVersion: 'v2.44.0',
  schemaRegistryHost: 'schema-registry.hasura.io',
  consoleType: 'oss',
  consoleMode: 'server',
  assetsPath: 'https://graphql-engine-cdn.hasura.io/console/assets',
  consolePath: '/console',
  enableTelemetry: false,
  isAdminSecretSet: true,
  urlPrefix: '/console',
  cdnAssets: false,
};

export const mockAppState: AppState = {
  readOnlyMode: false,
  serverVersion: 'v2.44.0',
  isProduction: false,
  latestServerVersion: {
    latest: 'v2.44.0',
    prerelease: '',
  },
  featuresCompatibility: {
    readOnlyRunSqlQueries: true,
  },
  envVars: mockEnvVars,
  endpoints: getEndpoints(mockEnvVars, baseUrl),
};
