// @vitest-environment jsdom
import { EnvVars } from '@hasura/shared/types';
import { getEndpoints } from './endpoints';

const baseGlobals = {
  apiHost: 'http://localhost',
  apiPort: '9693',
  luxDataHost: '',
  schemaRegistryHost: 'schema-registry.hasura.io',
} as EnvVars;

describe('getEndpoints', () => {
  it('derives http endpoints from the base URL', () => {
    const endpoints = getEndpoints(baseGlobals, 'http://localhost:8080');

    expect(endpoints.baseUrl).toBe('http://localhost:8080');
    expect(endpoints.graphQLUrl).toBe('http://localhost:8080/v1/graphql');
    expect(endpoints.relayURL).toBe('http://localhost:8080/v1/relay');
    expect(endpoints.metadata).toBe('http://localhost:8080/v1/metadata');
    expect(endpoints.version).toBe('http://localhost:8080/v1/version');
    expect(endpoints.query).toBe('http://localhost:8080/v2/query');
    expect(endpoints.queryV2).toBe('http://localhost:8080/v2/query');
    expect(endpoints.entitlement).toBe('http://localhost:8080/v1/entitlement');
    expect(endpoints.license).toBe(
      'http://localhost:8080/v1/entitlement/license',
    );
    expect(endpoints.serverConfig).toBe(
      'http://localhost:8080/v1alpha1/config',
    );
    expect(endpoints.prometheusUrl).toBe('http://localhost:8080/v1/metrics');
  });

  it('derives ws endpoints from http URLs', () => {
    const endpoints = getEndpoints(baseGlobals, 'http://localhost:8080');

    expect(endpoints.graphQLWebSocketURL).toBe(
      'ws://localhost:8080/v1/graphql',
    );
    expect(endpoints.relayWebSocketURL).toBe('ws://localhost:8080/v1/relay');
  });

  it('derives wss endpoints from https URLs', () => {
    const endpoints = getEndpoints(
      baseGlobals,
      'https://my-project.hasura.app',
    );

    expect(endpoints.graphQLWebSocketURL).toBe(
      'wss://my-project.hasura.app/v1/graphql',
    );
    expect(endpoints.relayWebSocketURL).toBe(
      'wss://my-project.hasura.app/v1/relay',
    );
  });

  it('builds the hasura-cli-server endpoints from apiHost/apiPort', () => {
    const endpoints = getEndpoints(baseGlobals, 'http://localhost:8080');

    expect(endpoints.hasuraCliServerMigrate).toBe(
      'http://localhost:9693/apis/migrate',
    );
    expect(endpoints.hasuraCliServerMetadata).toBe(
      'http://localhost:9693/apis/metadata',
    );
    expect(endpoints.hasuraCliServerMigrateSettings).toBe(
      'http://localhost:9693/apis/migrate/settings',
    );
  });

  // luxDataGraphql/luxDataGraphqlWs/schemaRegistry are built from
  // `window.location.protocol` rather than `baseUrl` (see the package's
  // CLAUDE.md gotcha: not SSR-safe, requires a browser environment). jsdom's
  // default location is http://localhost/, so these assertions use "http:".
  it('leaves luxDataGraphql host empty when luxDataHost is not set', () => {
    const endpoints = getEndpoints(baseGlobals, 'http://localhost:8080');

    expect(endpoints.luxDataGraphql).toBe('http:///v1/graphql');
  });

  it('strips a trailing slash from luxDataHost before building lux endpoints', () => {
    const endpoints = getEndpoints(
      { ...baseGlobals, luxDataHost: 'data.pro.hasura.io/' } as EnvVars,
      'https://my-project.hasura.app',
    );

    expect(endpoints.luxDataGraphql).toBe(
      'http://data.pro.hasura.io/v1/graphql',
    );
    expect(endpoints.luxDataGraphqlWs).toBe(
      'ws://data.pro.hasura.io/v1/graphql',
    );
  });

  it('builds the schema registry endpoint from schemaRegistryHost using the current page protocol', () => {
    const endpoints = getEndpoints(baseGlobals, 'http://localhost:8080');

    expect(endpoints.schemaRegistry).toBe(
      'http://schema-registry.hasura.io/v1/graphql',
    );
  });

  it('uses window.location.protocol (not baseUrl) for luxDataGraphql/schemaRegistry', () => {
    const originalLocation = window.location;
    // jsdom doesn't allow assigning to individual location properties
    // directly, so replace the whole object for the duration of this test.
    // @ts-expect-error - deleting to allow reassignment
    delete window.location;
    window.location = { ...originalLocation, protocol: 'https:' };

    try {
      const endpoints = getEndpoints(
        { ...baseGlobals, luxDataHost: 'data.pro.hasura.io' } as EnvVars,
        'http://localhost:8080', // note: baseUrl is still http
      );

      expect(endpoints.luxDataGraphql).toBe(
        'https://data.pro.hasura.io/v1/graphql',
      );
      expect(endpoints.luxDataGraphqlWs).toBe(
        'wss://data.pro.hasura.io/v1/graphql',
      );
      expect(endpoints.schemaRegistry).toBe(
        'https://schema-registry.hasura.io/v1/graphql',
      );
    } finally {
      window.location = originalLocation;
    }
  });

  it('always returns the same hardcoded external endpoints regardless of baseUrl', () => {
    const endpoints = getEndpoints(baseGlobals, 'http://localhost:8080');

    expect(endpoints.updateCheck).toBe(
      'https://releases.hasura.io/graphql-engine',
    );
    expect(endpoints.telemetryServer).toBe('wss://telemetry.hasura.io/v1/ws');
    expect(endpoints.consoleNotificationsProd).toBe(
      'https://notifications.hasura.io/v1/graphql',
    );
    expect(endpoints.consoleNotificationsStg).toBe(
      'https://notifications.hasura-stg.hasura-app.io/v1/graphql',
    );
    expect(endpoints.registerEETrial).toBe(
      'https://licensing.pro.hasura.io/v1/graphql',
    );
  });
});
