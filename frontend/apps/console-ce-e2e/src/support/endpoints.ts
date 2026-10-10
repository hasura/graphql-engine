/**
 * Central, test-only endpoint configuration for the E2E suite.
 *
 * The HGE and CLI-migrate base URLs default to the values CI uses
 * (`http://localhost:8080` and `http://localhost:9693`) so nothing changes for
 * CI or local runs that keep the defaults. They can be OVERRIDDEN explicitly via
 * Cypress env vars (e.g. `CYPRESS_HGE_URL` / `CYPRESS_CLI_URL`, or `--env
 * HGE_URL=...,CLI_URL=...`) when the backend is reachable on a different
 * host/port — for example a locally owned, isolated HGE when the standard ports
 * are taken.
 *
 * This only parameterises the suite's OWN requests/intercepts against the HGE
 * and CLI APIs. It must NOT be used to rewrite unrelated/external URLs (e.g. a
 * remote-schema GraphQL endpoint), and it does not intercept or suppress
 * anything by itself.
 */
const DEFAULT_HGE_URL = 'http://localhost:8080';
const DEFAULT_CLI_URL = 'http://localhost:9693';

const trimTrailingSlashes = (url: string): string => url.replace(/\/+$/, '');

// Join a base URL and a path with exactly one separator, while leaving
// query-strings and glob patterns (`?...`, `*`, `**`) attached directly.
const joinUrl = (base: string, path: string): string => {
  if (!path) return trimTrailingSlashes(base);
  const b = trimTrailingSlashes(base);
  if (path.startsWith('?')) return `${b}${path}`;
  const p = path.startsWith('/') ? path : `/${path}`;
  return `${b}${p}`;
};

const readEnv = (key: string): string | undefined => {
  // `Cypress` exists in the browser-run context; guard so the helper is also
  // importable by plain Node unit tests.
  if (typeof Cypress !== 'undefined') {
    const v = Cypress.env(key);
    if (typeof v === 'string' && v.length > 0) return v;
  }
  return undefined;
};

/** Base HGE URL (no trailing slash). Default `http://localhost:8080`. */
export const hgeBaseUrl = (): string =>
  trimTrailingSlashes(readEnv('HGE_URL') ?? DEFAULT_HGE_URL);

/** Base CLI-migrate URL (no trailing slash). Default `http://localhost:9693`. */
export const cliBaseUrl = (): string =>
  trimTrailingSlashes(readEnv('CLI_URL') ?? DEFAULT_CLI_URL);

/** HGE URL for a path, e.g. `hgeUrl('/v1/metadata')`. */
export const hgeUrl = (path = ''): string => joinUrl(hgeBaseUrl(), path);

/** CLI-migrate URL for a path, e.g. `cliUrl('/apis/migrate')`. */
export const cliUrl = (path = ''): string => joinUrl(cliBaseUrl(), path);

export const ENDPOINT_DEFAULTS = {
  HGE_URL: DEFAULT_HGE_URL,
  CLI_URL: DEFAULT_CLI_URL,
};

// Exported for the unit test only.
export const __test__ = { joinUrl, trimTrailingSlashes };
