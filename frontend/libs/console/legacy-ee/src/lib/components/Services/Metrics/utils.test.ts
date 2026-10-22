import Endpoints from '../../../Endpoints';
import { getMetricsUrl } from './utils';

// The real `Endpoints` module reads `globals.metricsApiUrl`, which the Cloud
// build ships empty so that `metricsGraphQLUrl` collapses to '/v1/graphql'.
// Mock it with a clearly distinct sentinel so these tests can tell the
// configured fallback apart from a URL derived from the supplied project host.
vi.mock('../../../Endpoints', () => ({
  default: {
    metricsGraphQLUrl: '/configured-fallback/v1/graphql',
  },
}));

describe('getMetricsUrl', () => {
  it('prefixes a bare project FQDN with https and the graphql path', () => {
    expect(getMetricsUrl('metrics.my-project.hasura.app')).toBe(
      'https://metrics.my-project.hasura.app/v1/graphql',
    );
  });

  it('preserves an explicit https project FQDN', () => {
    expect(getMetricsUrl('https://metrics.my-project.hasura.app')).toBe(
      'https://metrics.my-project.hasura.app/v1/graphql',
    );
  });

  it('preserves an explicit http project FQDN', () => {
    expect(getMetricsUrl('http://localhost:8080')).toBe(
      'http://localhost:8080/v1/graphql',
    );
  });

  it('falls back to the configured metrics endpoint when the FQDN is empty', () => {
    expect(getMetricsUrl('')).toBe(Endpoints.metricsGraphQLUrl);
    expect(getMetricsUrl('')).toBe('/configured-fallback/v1/graphql');
  });
});
