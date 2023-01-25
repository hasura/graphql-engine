import { extractPrometheusUrl } from './index';

describe('Prometheus URL extraction', () => {
  it.each`
    url                         | expected
    ${'http://localhost:8080'}  | ${['', 'http', 'localhost']}
    ${'https://localhost:8080'} | ${['', 'https', 'localhost']}
    ${'https://localhost'}      | ${['', 'https', 'localhost']}
    ${'http://hasura.io:8080'}  | ${['', 'http', 'hasura.io']}
    ${'https://hasura.io:8080'} | ${['', 'https', 'hasura.io']}
    ${'https://hasura.io'}      | ${['', 'https', 'hasura.io']}
  `('should return null for invalid URL $url', ({ url, expected }) => {
    const result = extractPrometheusUrl(url);
    expect(result).toBe(expected);
  });

  it.each`
    url
    ${'localhost:8080'}
    ${'localhost'}
    ${'hasura.io:8080'}
    ${'hasura.io'}
  `('should return default values for invalid URL $url', ({ url }) => {
    const result = extractPrometheusUrl(url);
    expect(result).toBe(['', 'http', 'myhasuradomain.com']);
  });
});
