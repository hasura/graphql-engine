import { extractPrometheusUrl } from './index';

// [1] is the scrape `scheme` and [2] the `targets` entry of the generated
// Prometheus config, so the port must be kept on the target.
describe('Prometheus URL extraction', () => {
  it.each`
    url                               | scheme     | target
    ${'http://localhost:8080'}        | ${'http'}  | ${'localhost:8080'}
    ${'https://localhost:8080'}       | ${'https'} | ${'localhost:8080'}
    ${'https://localhost'}            | ${'https'} | ${'localhost'}
    ${'http://hasura.io:8080'}        | ${'http'}  | ${'hasura.io:8080'}
    ${'https://hasura.io:8080'}       | ${'https'} | ${'hasura.io:8080'}
    ${'https://hasura.io'}            | ${'https'} | ${'hasura.io'}
    ${'https://hasura.io/v1/metrics'} | ${'https'} | ${'hasura.io'}
  `(
    'extracts scheme $scheme and target $target from $url',
    ({ url, scheme, target }) => {
      const result = extractPrometheusUrl(url);
      expect([result[1], result[2]]).toEqual([scheme, target]);
    },
  );

  it.each`
    url
    ${'localhost:8080'}
    ${'localhost'}
    ${'hasura.io:8080'}
    ${'hasura.io'}
  `('falls back to default values for URL without scheme $url', ({ url }) => {
    expect(extractPrometheusUrl(url)).toEqual([
      '',
      'http',
      'myhasuradomain.com',
    ]);
  });
});
