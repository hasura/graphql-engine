/**
 * No-backend unit test for the central endpoint helper. Runs inside Cypress via
 * the `src/support/**\/*unit.test.{js,ts}` specPattern — no HGE/console needed.
 * Covers the default URLs, explicit overrides, and safe path joining.
 */
import {
  hgeUrl,
  cliUrl,
  hgeBaseUrl,
  cliBaseUrl,
  ENDPOINT_DEFAULTS,
  __test__,
} from './endpoints';

describe('e2e endpoint helper', () => {
  // Capture whatever the run was invoked with (e.g. `--env HGE_URL=...`) so we can
  // restore it exactly as found and not leak a forced `undefined` into later code.
  let originalHgeUrl: unknown;
  let originalCliUrl: unknown;
  before(() => {
    originalHgeUrl = Cypress.env('HGE_URL');
    originalCliUrl = Cypress.env('CLI_URL');
  });
  // Start each test from a clean slate so the "defaults" assertions hold even
  // when the whole run is invoked with `--env HGE_URL=...,CLI_URL=...`.
  beforeEach(() => {
    Cypress.env('HGE_URL', undefined);
    Cypress.env('CLI_URL', undefined);
  });
  // Restore the env to exactly what it was before this suite ran.
  after(() => {
    Cypress.env('HGE_URL', originalHgeUrl);
    Cypress.env('CLI_URL', originalCliUrl);
  });

  it('defaults match CI (8080 / 9693)', () => {
    expect(hgeBaseUrl()).to.equal('http://localhost:8080');
    expect(cliBaseUrl()).to.equal('http://localhost:9693');
    expect(hgeUrl('/v1/metadata')).to.equal(
      'http://localhost:8080/v1/metadata',
    );
    expect(cliUrl('/apis/migrate')).to.equal(
      'http://localhost:9693/apis/migrate',
    );
    expect(ENDPOINT_DEFAULTS.HGE_URL).to.equal('http://localhost:8080');
  });

  it('honours explicit overrides', () => {
    Cypress.env('HGE_URL', 'http://localhost:18080');
    Cypress.env('CLI_URL', 'http://localhost:16993');
    expect(hgeUrl('/v1/metadata')).to.equal(
      'http://localhost:18080/v1/metadata',
    );
    expect(cliUrl('/apis/migrate')).to.equal(
      'http://localhost:16993/apis/migrate',
    );
  });

  it('joins paths safely (single separator, trailing slash + query + glob)', () => {
    Cypress.env('HGE_URL', 'http://localhost:18080/'); // trailing slash
    expect(hgeUrl('/v1/metadata')).to.equal(
      'http://localhost:18080/v1/metadata',
    );
    expect(hgeUrl('v1/metadata')).to.equal(
      'http://localhost:18080/v1/metadata',
    );
    expect(hgeUrl('/**')).to.equal('http://localhost:18080/**');
    expect(hgeUrl('?x=1')).to.equal('http://localhost:18080?x=1');
    expect(hgeUrl()).to.equal('http://localhost:18080');
  });

  it('joinUrl/trimTrailingSlashes unit behaviour', () => {
    expect(__test__.trimTrailingSlashes('http://h//')).to.equal('http://h');
    expect(__test__.joinUrl('http://h/', '/a')).to.equal('http://h/a');
    expect(__test__.joinUrl('http://h', 'a')).to.equal('http://h/a');
  });
});
